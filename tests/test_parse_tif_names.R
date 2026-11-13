# Quick self-test of the TIF-name parser using real LAS X export names.
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# Run from the pipeline folder:  source("tests/test_parse_tif_names.R")
source("R/utils.R"); source("R/parse_tif_names.R")

files <- c("SLIDE1-Series004_ch00.tif", "SLIDE1-Series004_ch01.tif", "SLIDE1-Series004_ch02.tif",
           "SLIDE1-Series004_ch03.tif", "SLIDE1-Series004_overlay.tif",
           "SLIDE3-2-Series004_ch00.tif", "SLIDE3-2-Series004_ch01.tif",
           "Series002.tif", "Series002_overlay.tif",
           "Series004_z00_overlay.tif", "Series004_z01_overlay.tif",
           sprintf("Series005_z%02d_ch%02d.tif", rep(0:10, each = 4), rep(0:3, 11)),
           "Series018_RAW_ch00.tif", "KO_63x_cell1_ch01.tif")

p <- parse_leica_tif_names(files)
stopifnot(
  p$base[p$file == "SLIDE3-2-Series004_ch00.tif"] == "SLIDE3-2-Series004",
  p$folder[p$file == "SLIDE3-2-Series004_ch00.tif"] == "SLIDE3-2",
  p$leaf[p$file == "SLIDE3-2-Series004_ch00.tif"] == "Series004",
  p$role[p$file == "Series002.tif"] == "all_channels",
  p$role[p$file == "Series004_z00_overlay.tif"] == "overlay",
  p$z[p$file == "Series005_z10_ch03.tif"] == 10,     # old macro broke at z >= 10
  p$ch[p$file == "Series005_z10_ch03.tif"] == 3,
  p$base[p$file == "Series018_RAW_ch00.tif"] == "Series018",
  p$image_type[p$file == "KO_63x_cell1_ch01.tif"] == "custom"
)

s <- summarise_tif_images(p)
print(s[, c("base", "layout", "n_files", "n_ch_files", "n_z_files", "file_status")])
stopifnot(
  s$layout[s$base == "Series005"] == "split_channels_z",
  s$file_status[s$base == "Series005"] == "ok",
  s$layout[s$base == "Series004"] == "overlay_only",
  grepl("fewer channels", s$file_status[s$base == "SLIDE3-2-Series004"])   # only 2 of 4 listed
)
cat("All parser tests passed.\n")
