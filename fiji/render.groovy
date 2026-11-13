#@ String jobs_tsv
#@ String out_dir

/*
 * render.groovy  —  Module 4 (Fiji side): apply saved B&C at FULL resolution
 * Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
 * (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
 * ---------------------------------------------------------------------------
 * For each image in jobs_tsv (written by R/render.R):
 *   1. max projection over z per channel (single planes pass through unchanged)
 *   2. <base>_composite.tif  multi-channel, UNALTERED pixel values (use for
 *      quantification); channel colors + min/max are stored as display settings
 *      only, so it opens in Fiji looking like the PNGs
 *   3. <base>_C<n>-<label>.png  each channel, 8-bit, linear min/max (gray or color)
 *   4. <base>_merge.png      additive RGB merge of the "in merge" channels
 *   5. optional <base>_stack.tif  the full z-stack (all channels), unaltered
 * The 8-bit conversion is ImageJ's own convertToByte(true) on the display range,
 * i.e. exactly what setMinAndMax() shows in Fiji. Inputs are only read.
 *
 * jobs_tsv columns: uid_safe out_base kind path series channel z labels mins maxs
 *                   colors in_merge pixel_size_um channel_png save_stack
 */

import ij.IJ
import ij.ImagePlus
import ij.ImageStack
import ij.CompositeImage
import ij.io.FileSaver
import ij.measure.Calibration
import ij.process.*
import java.awt.Color
import loci.plugins.BF
import loci.plugins.in.ImporterOptions

// ---- built-in .lif reader: reads pixels straight from the memory block that R located
// by ID (R/lif_reader.R), using Leica's own byte layout. Little-endian, 8 or 16 bit.
@groovy.transform.CompileStatic
class LifRaw {
    static Map<String, String> parse(String s) {
        Map<String, String> m = new HashMap<String, String>()
        for (String kv : s.split(";")) { int i = kv.indexOf('='); if (i > 0) m.put(kv.substring(0, i), kv.substring(i + 1)) }
        return m
    }
    static ImageProcessor plane(RandomAccessFile raf, long base, int sx, int sy, long bx, long by, int bps) {
        int n = sx * sy
        short[] s16 = bps == 2 ? new short[n] : null
        byte[] s8 = bps == 1 ? new byte[n] : null
        if (bx == (long) bps && by == ((long) sx) * bps) {          // contiguous plane: one read
            byte[] buf = new byte[n * bps]
            raf.seek(base); raf.readFully(buf)
            if (bps == 2) { for (int i = 0; i < n; i++) s16[i] = (short) ((buf[2 * i] & 0xff) | ((buf[2 * i + 1] & 0xff) << 8)) }
            else System.arraycopy(buf, 0, s8, 0, n)
        } else {                                                      // strided: line by line
            int span = (int) ((sx - 1) * bx + bps)
            byte[] line = new byte[span]
            for (int y = 0; y < sy; y++) {
                raf.seek(base + y * by); raf.readFully(line)
                int o = y * sx
                for (int x = 0; x < sx; x++) {
                    int p = (int) (x * bx)
                    if (bps == 2) s16[o + x] = (short) ((line[p] & 0xff) | ((line[p + 1] & 0xff) << 8))
                    else s8[o + x] = line[p]
                }
            }
        }
        if (bps == 2) return new ShortProcessor(sx, sy, s16, null)
        return new ByteProcessor(sx, sy, s8, null)
    }
}

// Fast pixel loops (compiled like Java). Integer math only: in Groovy, float arithmetic
// is promoted to double, which static compilation rejects.
@groovy.transform.CompileStatic
class Px {
    // R/G/B accumulate (0-255 value) x (0-255 color component); divided by 255 at the end
    static void addColor(byte[] src, int[] R, int[] G, int[] B, int r, int g, int b) {
        for (int i = 0; i < src.length; i++) {
            int v = src[i] & 0xff
            R[i] += v * r; G[i] += v * g; B[i] += v * b
        }
    }
    static ColorProcessor toRgb(int w, int h, int[] R, int[] G, int[] B) {
        byte[] rb = new byte[w * h], gb = new byte[w * h], bb = new byte[w * h]
        for (int i = 0; i < rb.length; i++) {
            rb[i] = (byte) Math.min(255, Math.floorDiv(R[i] + 127, 255))
            gb[i] = (byte) Math.min(255, Math.floorDiv(G[i] + 127, 255))
            bb[i] = (byte) Math.min(255, Math.floorDiv(B[i] + 127, 255))
        }
        ColorProcessor cp = new ColorProcessor(w, h); cp.setRGB(rb, gb, bb); return cp
    }
    static double fractionAtOrAbove(float[] px, double hi) {
        long n = 0L
        for (int i = 0; i < px.length; i++) { if (px[i] >= hi) n++ }
        return ((double) n) / ((double) px.length)
    }
}

// ---- read jobs ---------------------------------------------------------------
def lines  = new File(jobs_tsv).readLines("UTF-8")
def header = lines[0].split("\t", -1) as List
def jobs   = new LinkedHashMap<String, List<Map>>()
lines.drop(1).findAll { it.trim() }.each { ln ->
    def f = ln.split("\t", -1)
    def row = [:]; header.eachWithIndex { h, i -> row[h] = i < f.length ? f[i] : "" }
    jobs.computeIfAbsent(row.uid_safe, { k -> [] }).add(row)
}

def hexColor = { String h ->
    try { return new Color(Integer.parseInt(h.trim().replace("#", ""), 16)) } catch (Exception e) { return Color.WHITE }
}
def maxInto = { ImageProcessor acc, ImageProcessor ip ->
    if (acc == null) return ip.duplicate()
    acc.copyBits(ip, 0, 0, Blitter.MAX); return acc
}

// returns [List<ImageProcessor> projections, ImagePlus source-or-null, nZ]
def projectBF = { String path, int series ->
    def opts = new ImporterOptions()
    opts.setId(path); opts.setVirtual(true); opts.setQuiet(true)
    opts.clearSeries(); opts.setSeriesOn(series, true)
    def imp = BF.openImagePlus(opts)[0]
    int nC = imp.getNChannels(), nZ = imp.getNSlices()
    def chans = []
    for (int c = 1; c <= nC; c++) {
        ImageProcessor acc = null
        for (int z = 1; z <= nZ; z++) acc = maxInto(acc, imp.getStack().getProcessor(imp.getStackIndex(c, z, 1)))
        chans << acc
    }
    return [chans, imp, nZ]
}
// returns [per-channel max projections, hyperstack ImagePlus or null, nZ]
def projectLifRaw = { Map row, boolean keepStack ->
    Map<String, String> L = LifRaw.parse(row.lif as String)
    long off = L["off"] as long; int bps = L["bps"] as int
    int sx = L["sx"] as int, sy = L["sy"] as int, sz = L["sz"] as int
    long bx = L["bx"] as long, by = L["by"] as long, bz = L["bz"] as long, bm = L["bm"] as long
    int m = (L["m"] ?: "0") as int
    List<Long> cinc = L["cinc"].split(",").collect { it as long }
    int nC = cinc.size()
    def planes = keepStack ? new ImageProcessor[sz][nC] : null
    def chans = []
    def raf = new RandomAccessFile(row.path as String, "r")
    try {
        for (int c = 0; c < nC; c++) {
            ImageProcessor acc = null
            for (int z = 0; z < sz; z++) {
                def ip = LifRaw.plane(raf, off + cinc[c] + z * bz + m * bm, sx, sy, bx, by, bps)
                if (keepStack) planes[z][c] = ip
                acc = maxInto(acc, ip)
            }
            chans << acc
        }
    } finally { raf.close() }
    ImagePlus imp = null
    if (keepStack) {
        def st = new ImageStack(sx, sy)
        for (int z = 0; z < sz; z++) for (int c = 0; c < nC; c++) st.addSlice(planes[z][c])
        imp = new ImagePlus("stack", st); imp.setDimensions(nC, sz, 1); imp.setOpenAsHyperStack(true)
    }
    return [chans, imp, sz]
}

def projectSplit = { List<Map> rows ->
    def byCh = new TreeMap<Integer, List<Map>>()
    rows.each { r -> byCh.computeIfAbsent(r.channel as Integer, { k -> [] }).add(r) }
    def chans = []; int nz = 0
    byCh.each { ch, rs ->
        ImageProcessor acc = null
        rs.sort { (it.z ?: "0") as Integer }.each { r ->
            def imp = IJ.openImage(r.path)
            if (imp == null) throw new RuntimeException("Could not open " + r.path)
            for (int s = 1; s <= imp.getStackSize(); s++) acc = maxInto(acc, imp.getStack().getProcessor(s))
            nz = Math.max(nz, rs.size() * imp.getStackSize()); imp.close()
        }
        chans << acc
    }
    return [chans, null, nz]
}

// ---- outputs -------------------------------------------------------------------
def wIdx = new PrintWriter(new OutputStreamWriter(new FileOutputStream(new File(out_dir, "render_run.tsv")), "UTF-8"))
wIdx.println(["uid_safe", "n_channels", "n_z", "width", "height", "files", "saturation_pct", "error"].join("\t"))

int done = 0, failed = 0
jobs.each { uid, rows ->
    ImagePlus src = null
    try {
        def j = rows[0]
        def labels = j.labels.split(";", -1); def mins = j.mins.split(";").collect { it as double }
        def maxs = j.maxs.split(";").collect { it as double }; def colors = j.colors.split(";").collect { hexColor(it) }
        def inMerge = j.in_merge.split(";").collect { it == "1" }
        def res = j.kind == "lifraw" ? projectLifRaw(j, j.save_stack == "1") :
                  (j.kind == "split" ? projectSplit(rows) : projectBF(j.path, (j.series ?: "0") as Integer))
        List<ImageProcessor> chans = res[0]; src = res[1]; int nZ = res[2]
        int W = chans[0].getWidth(), H = chans[0].getHeight(), nC = chans.size()
        new File(j.out_base).getParentFile().mkdirs()
        def files = []; def sat = []

        // 1) composite TIF: unaltered data + display settings
        def stack = new ImageStack(W, H)
        chans.eachWithIndex { ip, c -> stack.addSlice(c < labels.size() ? labels[c] : "C" + (c + 1), ip) }
        ImagePlus comp = new ImagePlus(new File(j.out_base).name, stack)
        comp.setDimensions(nC, 1, 1)
        if (nC > 1) comp = new CompositeImage(comp, IJ.COMPOSITE)
        double px = (j.pixel_size_um ?: "0") as double
        if (px > 0) { def cal = new Calibration(); cal.pixelWidth = px; cal.pixelHeight = px; cal.setUnit("micron"); comp.setCalibration(cal) }
        for (int c = 0; c < nC; c++) {
            if (c >= mins.size()) break
            if (nC > 1) { ((CompositeImage) comp).setChannelLut(LUT.createLutFromColor(colors[c]), c + 1); comp.setPosition(c + 1, 1, 1) }
            else comp.getProcessor().setLut(LUT.createLutFromColor(colors[c]))
            comp.setDisplayRange(mins[c], maxs[c])
        }
        if (nC > 1) comp.setPosition(1, 1, 1)
        IJ.saveAsTiff(comp, j.out_base + "_composite.tif"); files << (j.out_base + "_composite.tif")

        // 2) per-channel PNGs + 3) merge
        int[] R = new int[W * H], G = new int[W * H], B = new int[W * H]
        for (int c = 0; c < nC && c < mins.size(); c++) {
            def raw = chans[c]
            sat << String.format("%.2f", 100 * Px.fractionAtOrAbove((float[]) raw.convertToFloatProcessor().getPixels(), maxs[c]))
            def d = raw.duplicate(); d.setMinAndMax(mins[c], maxs[c])
            ByteProcessor bp = (ByteProcessor) d.convertToByte(true)          // linear, same as Fiji display
            bp.setLut(LUT.createLutFromColor(Color.WHITE))   // drop LAS X's LUT carried over from the .lif -> true grayscale
            String lab = (labels[c] ?: "C" + (c + 1)).replaceAll(/[^A-Za-z0-9_.-]+/, "_")
            String f = j.out_base + "_C" + (c + 1) + "-" + lab + ".png"
            if (j.channel_png == "color") {
                def cb = bp.duplicate(); cb.setLut(LUT.createLutFromColor(colors[c]))
                new FileSaver(new ImagePlus("c", cb.convertToRGB())).saveAsPng(f)
            } else new FileSaver(new ImagePlus("c", bp)).saveAsPng(f)
            files << f
            if (inMerge[c]) Px.addColor((byte[]) bp.getPixels(), R, G, B, colors[c].getRed(), colors[c].getGreen(), colors[c].getBlue())
        }
        new FileSaver(new ImagePlus("merge", Px.toRgb(W, H, R, G, B))).saveAsPng(j.out_base + "_merge.png")
        files << (j.out_base + "_merge.png")

        // 4) optional full z-stack
        if (j.save_stack == "1" && src != null && nZ > 1) {
            IJ.saveAsTiff(src, j.out_base + "_stack.tif"); files << (j.out_base + "_stack.tif")
        }
        wIdx.println([uid, nC, nZ, W, H, files.collect { new File(it).name }.join(";"), sat.join(";"), ""].join("\t"))
        wIdx.flush(); done++
        println "[render] ${uid}: ${nC} ch, ${nZ} z, ${W}x${H} -> ${files.size()} files"
    } catch (Throwable t) {
        failed++
        println "[render] ERROR ${uid}: ${t}"
        wIdx.println([uid, "", "", "", "", "", "", t.toString().replaceAll(/[\t\r\n]+/, " ")].join("\t")); wIdx.flush()
    } finally {
        if (src != null) src.close()
    }
}
wIdx.close()
new File(out_dir, "render_done.txt").text = "done=${done}\nfailed=${failed}\n"
println "[render] finished: ${done} ok, ${failed} failed"
