#@ String input_path
#@ String out_dir
#@ Boolean (value=true) dump_original_metadata
#@ Boolean (value=true) skip_overlay
#@ String (value="") file_extensions

/*
 * inventory.groovy  —  Module 1 (Fiji side)
 * Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
 * (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
 * ---------------------------------------------------------------------------
 * Reads one image file (any format Bio-Formats knows: .lif, .czi, .nd2, ...), a folder
 * of such files (file_extensions = "czi,nd2,..."), or a folder of LAS X .tif exports, and
 * writes plain tables describing what is in it. Reads METADATA ONLY — no pixel
 * data is loaded and nothing in the input is modified.
 *
 * Run headless from R (R/fiji.R) or by hand:
 *   <Fiji launcher> --headless --console --run inventory.groovy \
 *       'input_path="/path/Project.lif",out_dir="/path/out"'
 *
 * Outputs (in out_dir):
 *   inventory_images.csv     one row per series (lif) or per file (tif folder)
 *   inventory_channels.csv   one row per series x channel
 *   original_metadata.tsv    every Leica key/value Bio-Formats exposes
 *                            (series_index = -1 means file-level/global)
 *   inventory_done.txt       written last; R checks for it
 *
 * Written in Groovy (Java syntax, runs inside Fiji) because the ImageJ macro
 * language cannot reach objective/laser metadata.
 */

import loci.formats.ImageReader
import loci.formats.MetadataTools
import loci.formats.FormatTools
import loci.formats.in.DefaultMetadataOptions
import loci.formats.in.MetadataLevel
import ome.units.UNITS

// ---------- helpers ---------------------------------------------------------
def safe = { Closure c -> try { return c() } catch (Throwable t) { return null } }

def toUnit = { len, unit ->
    if (len == null) return null
    def v = safe { len.value(unit) }
    if (v == null) v = safe { len.value() }          // fall back to raw value
    return v == null ? null : v.doubleValue()
}

def csvq = { v ->
    if (v == null) return ""
    def s = v.toString()
    if (s.contains(",") || s.contains("\"") || s.contains("\n") || s.contains("\r"))
        return "\"" + s.replace("\"", "\"\"") + "\""
    return s
}
def csvRow = { List vals -> vals.collect { csvq(it) }.join(",") }
def tsvClean = { v -> v == null ? "" : v.toString().replaceAll(/[\t\r\n]+/, " ") }

def objectiveFor = { meta, int s ->
    def res = [model: null, mag: null, na: null, imm: null]
    def objId = safe { meta.getObjectiveSettingsID(s) }
    int nInst = (safe { meta.getInstrumentCount() } ?: 0) as int
    for (int i = 0; i < nInst; i++) {
        int nObj = (safe { meta.getObjectiveCount(i) } ?: 0) as int
        for (int o = 0; o < nObj; o++) {
            def id = safe { meta.getObjectiveID(i, o) }
            boolean hit = (objId != null && id == objId) || (objId == null && nInst == 1 && nObj == 1)
            if (hit) {
                res.model = safe { meta.getObjectiveModel(i, o) }
                res.mag   = safe { meta.getObjectiveNominalMagnification(i, o) }
                res.na    = safe { meta.getObjectiveLensNA(i, o) }
                res.imm   = safe { meta.getObjectiveImmersion(i, o)?.toString() }
                return res
            }
        }
    }
    return res
}

// ---------- inputs ----------------------------------------------------------
def inFile = new File(input_path)
def outDirFile = new File(out_dir)
outDirFile.mkdirs()

String mode
List<File> targets
def exts = file_extensions.split(",").collect { it.trim().toLowerCase() }.findAll { it }
if (inFile.isDirectory() && exts) {
    mode = "bioformats"
    targets = inFile.listFiles().findAll { f ->
        f.isFile() && !f.name.startsWith(".") && exts.any { e -> f.name.toLowerCase().endsWith("." + e) }
    }.sort { it.name }
} else if (inFile.isDirectory()) {
    mode = "tif_folder"
    targets = inFile.listFiles().findAll {
        it.isFile() && !it.name.startsWith(".") && it.name ==~ /(?i).*\.tiff?$/ &&
        !(skip_overlay && it.name ==~ /(?i).*_overlay\.tiff?$/)
    }.sort { it.name }
} else if (inFile.isFile()) {
    mode = inFile.name.toLowerCase().endsWith(".lif") ? "lif" : "bioformats"
    targets = [inFile]
} else {
    throw new IllegalArgumentException("Input not found: " + input_path)
}

def openW = { String name -> new PrintWriter(new OutputStreamWriter(new FileOutputStream(new File(outDirFile, name)), "UTF-8")) }
def wImg  = openW("inventory_images.csv")
def wCh   = openW("inventory_channels.csv")
def wMeta = openW("original_metadata.tsv")

wImg.println(csvRow(["source_file","source_mode","series_index","series_name",
    "size_x","size_y","size_z","size_c","size_t","pixel_type","bits_per_pixel","is_rgb",
    "pixel_size_x_um","pixel_size_y_um","z_step_um","acquisition_date",
    "objective_model","objective_mag","objective_na","objective_immersion",
    "stage_x","stage_y","stage_z","dimension_order","read_error"]))
wCh.println(csvRow(["source_file","series_index","channel_index","channel_name",
    "excitation_nm","emission_nm","ome_attenuation","detector_gain","lif_color",
    "exposure_ms","fluor","acquisition_mode"]))
wMeta.println(["source_file","series_index","key","value"].join("\t"))

int nRows = 0
int nErrors = 0

// ---------- main loop -------------------------------------------------------
targets.each { File f ->
    def reader = new ImageReader()
    def meta = MetadataTools.createOMEXMLMetadata()
    try {
        reader.setMetadataOptions(new DefaultMetadataOptions(MetadataLevel.ALL))
        reader.setGroupFiles(false)          // never merge separate TIFs into one dataset
        reader.setMetadataStore(meta)
        reader.setId(f.absolutePath)
        int nSeries = reader.getSeriesCount()
        println "[inventory] ${f.name}: ${nSeries} series"

        if (dump_original_metadata && mode == "lif") {
            safe { reader.getGlobalMetadata() }?.each { k, v ->
                wMeta.println([f.absolutePath, "-1", tsvClean(k), tsvClean(v)].join("\t"))
            }
        }

        for (int s = 0; s < nSeries; s++) {
            reader.setSeries(s)
            def obj = objectiveFor(meta, s)
            def name = safe { meta.getImageName(s) } ?: (mode == "lif" ? "series_" + s : f.name)

            wImg.println(csvRow([
                f.absolutePath, mode, s, name,
                reader.getSizeX(), reader.getSizeY(), reader.getSizeZ(),
                reader.getSizeC(), reader.getSizeT(),
                FormatTools.getPixelTypeString(reader.getPixelType()),
                reader.getBitsPerPixel(), reader.isRGB(),
                toUnit(safe { meta.getPixelsPhysicalSizeX(s) }, UNITS.MICROMETER),
                toUnit(safe { meta.getPixelsPhysicalSizeY(s) }, UNITS.MICROMETER),
                toUnit(safe { meta.getPixelsPhysicalSizeZ(s) }, UNITS.MICROMETER),
                safe { meta.getImageAcquisitionDate(s)?.getValue() },
                obj.model, obj.mag, obj.na, obj.imm,
                toUnit(safe { meta.getPlanePositionX(s, 0) }, UNITS.MICROMETER),
                toUnit(safe { meta.getPlanePositionY(s, 0) }, UNITS.MICROMETER),
                toUnit(safe { meta.getPlanePositionZ(s, 0) }, UNITS.MICROMETER),
                reader.getDimensionOrder(), ""
            ]))
            nRows++

            int nCh = (safe { meta.getChannelCount(s) } ?: reader.getEffectiveSizeC()) as int
            int nPl = (safe { meta.getPlaneCount(s) } ?: 0) as int
            for (int c = 0; c < nCh; c++) {
                def expo = null                      // exposure of the first plane of this channel
                for (int p = 0; p < nPl; p++) {
                    if (safe { meta.getPlaneTheC(s, p)?.getValue() } == c) { expo = safe { meta.getPlaneExposureTime(s, p) }; break }
                }
                def col = safe { meta.getChannelColor(s, c) }
                def hex = col == null ? null : String.format("#%02X%02X%02X", col.getRed(), col.getGreen(), col.getBlue())
                wCh.println(csvRow([
                    f.absolutePath, s, c,
                    safe { meta.getChannelName(s, c) },
                    toUnit(safe { meta.getChannelExcitationWavelength(s, c) }, UNITS.NANOMETER),
                    toUnit(safe { meta.getChannelEmissionWavelength(s, c) }, UNITS.NANOMETER),
                    safe { meta.getChannelLightSourceSettingsAttenuation(s, c)?.getValue() },
                    safe { meta.getDetectorSettingsGain(s, c) },
                    hex,
                    toUnit(expo, UNITS.MILLISECOND),
                    safe { meta.getChannelFluor(s, c) },
                    safe { meta.getChannelAcquisitionMode(s, c)?.toString() }
                ]))
            }

            if (dump_original_metadata) {
                safe { reader.getSeriesMetadata() }?.each { k, v ->
                    wMeta.println([f.absolutePath, s.toString(), tsvClean(k), tsvClean(v)].join("\t"))
                }
            }
        }
    } catch (Throwable t) {
        nErrors++
        println "[inventory] ERROR reading ${f.name}: ${t}"
        wImg.println(csvRow([f.absolutePath, mode, "", "", "", "", "", "", "", "", "", "",
            "", "", "", "", "", "", "", "", "", "", "", "", t.toString()]))
    } finally {
        safe { reader.close() }
    }
}

wImg.close(); wCh.close(); wMeta.close()
new File(outDirFile, "inventory_done.txt").text =
    "mode=${mode}\nfiles=${targets.size()}\nrows=${nRows}\nerrors=${nErrors}\n"
println "[inventory] done: ${targets.size()} file(s), ${nRows} row(s), ${nErrors} error(s)"
