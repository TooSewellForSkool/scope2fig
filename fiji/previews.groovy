#@ String jobs_tsv
#@ String out_dir
#@ Integer (value=512) max_size

/*
 * previews.groovy  —  thumbnails + small raw previews (Fiji side)
 * Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
 * (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
 * ---------------------------------------------------------------------------
 * For each image in jobs_tsv (written by R/previews.R):
 *   1. max-intensity projection over z, per channel, at FULL resolution
 *   2. per-channel stats from that projection (min, max, mean, 0.1% / 99.9%)
 *   3. downsample to <= max_size px
 *   4. raw/<uid>.tif     multi-channel, original bit depth, NOT contrast adjusted
 *                        (used for the live B&C preview in the next module)
 *      thumbs/<uid>.png  RGB composite, auto-contrast per channel - for
 *                        recognising images only, never for analysis
 * Inputs are only read. jobs_tsv columns:
 *   uid_safe  kind(bf|split|lifraw)  path  series  channel  z  colors(#RRGGBB;...)  max_size
 */

import ij.IJ
import ij.ImagePlus
import ij.ImageStack
import ij.io.FileSaver
import ij.process.Blitter
import ij.process.ColorProcessor
import ij.process.FloatProcessor
import ij.process.ImageProcessor
import loci.plugins.BF
import loci.plugins.in.ImporterOptions
import ij.process.ByteProcessor
import ij.process.ShortProcessor

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

def outRoot  = new File(out_dir)
def rawDir   = new File(outRoot, "raw");    rawDir.mkdirs()
def thumbDir = new File(outRoot, "thumbs"); thumbDir.mkdirs()

// ---- read jobs ---------------------------------------------------------------
def lines  = new File(jobs_tsv).readLines("UTF-8")
def header = lines[0].split("\t", -1) as List
def jobs   = new LinkedHashMap<String, List<Map>>()
lines.drop(1).findAll { it.trim() }.each { ln ->
    def f = ln.split("\t", -1)
    def row = [:]; header.eachWithIndex { h, i -> row[h] = i < f.length ? f[i] : "" }
    jobs.computeIfAbsent(row.uid_safe, { k -> [] }).add(row)
}

// ---- helpers -----------------------------------------------------------------
def maxInto = { ImageProcessor acc, ImageProcessor ip ->
    if (acc == null) return ip.duplicate()
    acc.copyBits(ip, 0, 0, Blitter.MAX); return acc
}

def percentiles = { FloatProcessor fp, double lo, double hi ->
    float[] px = (float[]) fp.getPixels()
    int n = px.length
    int step = Math.max(1, (int) (n / 200000))
    float[] s = new float[(int) Math.ceil(n / (double) step)]
    int k = 0
    for (int i = 0; i < n; i += step) s[k++] = px[i]
    Arrays.sort(s, 0, k)
    return [s[(int) Math.floor(lo * (k - 1))], s[(int) Math.floor(hi * (k - 1))]]
}

def hexToRgb = { String h ->
    try { int v = Integer.parseInt(h.trim().replace("#", ""), 16); return [(v >> 16) & 255, (v >> 8) & 255, v & 255] }
    catch (Exception e) { return [255, 255, 255] }
}

// per-channel full-resolution max projections
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
    def info = [nz: nZ, w: imp.getWidth(), h: imp.getHeight()]
    imp.close()
    return [chans, info]
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
    def chans = []; int nz = 0; int w = 0, h = 0
    byCh.each { ch, rs ->
        ImageProcessor acc = null
        rs.sort { (it.z ?: "0") as Integer }.each { r ->
            def imp = IJ.openImage(r.path)
            if (imp == null) throw new RuntimeException("Could not open " + r.path)
            for (int s = 1; s <= imp.getStackSize(); s++) acc = maxInto(acc, imp.getStack().getProcessor(s))
            nz = Math.max(nz, rs.size() * imp.getStackSize()); w = imp.getWidth(); h = imp.getHeight()
            imp.close()
        }
        chans << acc
    }
    return [chans, [nz: nz, w: w, h: h]]
}

// ---- outputs -------------------------------------------------------------------
def wIdx = new PrintWriter(new OutputStreamWriter(new FileOutputStream(new File(outRoot, "preview_index_run.tsv")), "UTF-8"))
def wSt  = new PrintWriter(new OutputStreamWriter(new FileOutputStream(new File(outRoot, "preview_stats_run.tsv")), "UTF-8"))
wIdx.println(["uid_safe", "n_channels", "full_width", "full_height", "n_z", "preview_width", "preview_height", "error"].join("\t"))
wSt.println(["uid_safe", "channel_index", "min", "max", "mean", "p001", "p999"].join("\t"))

int done = 0, failed = 0
jobs.each { uid, rows ->
    try {
        def first = rows[0]
        def res
        if (first.kind == "lifraw") {
            def r3 = projectLifRaw(first, false); List<ImageProcessor> cc = r3[0]
            res = [cc, [nz: r3[2], w: cc[0].getWidth(), h: cc[0].getHeight()]]
        } else res = first.kind == "split" ? projectSplit(rows) : projectBF(first.path, (first.series ?: "0") as Integer)
        List<ImageProcessor> chans = res[0]; def info = res[1]
        def colors = (first.colors ?: "").split(";").findAll { it }
        int W = chans[0].getWidth(), H = chans[0].getHeight()
        int ms = (first.max_size ?: "") ? (first.max_size as Integer) : max_size     // per-image size (large scans get more)
        double sc = Math.min(1.0d, ms / (double) Math.max(W, H))
        int nw = Math.max(1, (int) Math.round(W * sc)), nh = Math.max(1, (int) Math.round(H * sc))

        def stack = new ImageStack(nw, nh)
        float[] R = new float[nw * nh], G = new float[nw * nh], B = new float[nw * nh]
        chans.eachWithIndex { ImageProcessor ip, int ci ->
            def full = ip.convertToFloatProcessor()
            def st = full.getStatistics()
            def p = percentiles(full, 0.001d, 0.999d)
            wSt.println([uid, ci + 1, st.min, st.max, st.mean, p[0], p[1]].join("\t"))

            ip.setInterpolationMethod(ImageProcessor.BILINEAR)
            def small = ip.resize(nw, nh, true)
            stack.addSlice("C" + (ci + 1), small)

            double lo = p[0], hi = p[1] > p[0] ? p[1] : p[0] + 1
            def rgb = hexToRgb(ci < colors.size() ? colors[ci] : "#FFFFFF")
            float[] px = (float[]) small.convertToFloatProcessor().getPixels()
            for (int i = 0; i < px.length; i++) {
                double v = (px[i] - lo) / (hi - lo)
                v = v < 0 ? 0 : (v > 1 ? 1 : v)
                R[i] += v * rgb[0]; G[i] += v * rgb[1]; B[i] += v * rgb[2]
            }
        }
        IJ.saveAsTiff(new ImagePlus(uid, stack), new File(rawDir, uid + ".tif").absolutePath)

        byte[] rb = new byte[nw * nh], gb = new byte[nw * nh], bb = new byte[nw * nh]
        for (int i = 0; i < rb.length; i++) {
            rb[i] = (byte) Math.min(255, (int) R[i]); gb[i] = (byte) Math.min(255, (int) G[i]); bb[i] = (byte) Math.min(255, (int) B[i])
        }
        def cp = new ColorProcessor(nw, nh); cp.setRGB(rb, gb, bb)
        new FileSaver(new ImagePlus(uid, cp)).saveAsPng(new File(thumbDir, uid + ".png").absolutePath)

        wIdx.println([uid, chans.size(), info.w, info.h, info.nz, nw, nh, ""].join("\t"))
        done++
        println "[previews] ${uid}: ${chans.size()} ch, ${info.nz} z -> ${nw}x${nh}"
    } catch (Throwable t) {
        failed++
        println "[previews] ERROR ${uid}: ${t}"
        wIdx.println([uid, "", "", "", "", "", "", t.toString().replaceAll(/[\t\r\n]+/, " ")].join("\t"))
    }
}
wIdx.close(); wSt.close()
new File(outRoot, "previews_done.txt").text = "done=${done}\nfailed=${failed}\n"
println "[previews] finished: ${done} ok, ${failed} failed"
