# parse_tif_names.R — understand LAS X TIF export names (fallback input mode)
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# Handles the patterns seen in LAS X exports:
#   SLIDE1-Series004_ch00.tif        folder "SLIDE1", one file per channel
#   Series005_z03_ch02.tif           one file per z-plane per channel
#   Series002.tif                    all channels in one file
#   Series018_RAW_ch00.tif           "RAW" export suffix
#   *_overlay.tif                    LAS X RGB merge -> always skipped
# LAS X joins folder and image name with "-", so "SLIDE3-2-Series004" = folder
# "SLIDE3-2" (or SLIDE3/2), image "Series004". Identity is the FULL base name.

TIF_NAME_REGEX <- paste0(
  "(?i)^(?<base>.+?)(?<raw>_RAW)?",
  "(?:_z(?<z>\\d+))?(?:_t(?<t>\\d+))?(?:_ch(?<ch>\\d+))?",
  "(?<overlay>_overlay)?\\.(?<ext>tiff?)$"
)
LEAF_REGEX <- "(?i)^(?:(?<folder>.*)-)?(?<leaf>(?<type>Series|Image|Region|Position|TileScan|Mark_and_Find)[ _]?\\d+)$"

parse_leica_tif_names <- function(files) {
  p <- regex_named(basename(files), TIF_NAME_REGEX)
  p$file <- basename(files)
  p$role <- ifelse(is.na(p$base), "unrecognized",
            ifelse(!is.na(p$overlay), "overlay",
            ifelse(!is.na(p$ch), "channel", "all_channels")))
  p$z  <- suppressWarnings(as.integer(p$z))
  p$t  <- suppressWarnings(as.integer(p$t))
  p$ch <- suppressWarnings(as.integer(p$ch))
  lf <- regex_named(ifelse(is.na(p$base), "", p$base), LEAF_REGEX)
  p$folder <- ifelse(is.na(lf$leaf), "", ifelse(is.na(lf$folder), "", lf$folder))
  p$leaf   <- ifelse(is.na(lf$leaf), p$base, lf$leaf)
  p$image_type <- ifelse(is.na(lf$type), "custom", tools::toTitleCase(tolower(lf$type)))
  p[, c("file", "base", "folder", "leaf", "image_type", "role", "z", "t", "ch", "raw", "ext")]
}

# One row per image: layout, completeness, file list
summarise_tif_images <- function(parsed) {
  bases <- unique(parsed$base[!is.na(parsed$base)])
  rows <- lapply(bases, function(b) {
    f    <- parsed[!is.na(parsed$base) & parsed$base == b, ]
    data <- f[f$role %in% c("channel", "all_channels"), ]
    n_ch <- length(unique(stats::na.omit(data$ch)))
    zs   <- unique(stats::na.omit(data$z))
    n_z  <- length(zs)
    layout <- if (nrow(data) == 0) "overlay_only"
              else if (all(data$role == "channel") && n_z > 0) "split_channels_z"
              else if (all(data$role == "channel")) "split_channels"
              else if (nrow(data) == 1) "single_file"
              else "mixed"
    expected <- switch(layout, split_channels_z = n_ch * n_z, split_channels = n_ch,
                       single_file = 1, NA)
    status <- if (layout == "overlay_only") "no raw channels found (overlay only)"
              else if (layout == "mixed") "mixed export layouts - check files"
              else if (!is.na(expected) && nrow(data) != expected)
                sprintf("incomplete: %d of %d files", nrow(data), expected)
              else if (n_ch > 0 && !identical(sort(unique(data$ch)), 0:(n_ch - 1)))
                "channel numbers not contiguous"
              else if (n_z > 0 && !identical(sort(zs), 0:(n_z - 1)))
                "z numbers not contiguous"
              else "ok"
    first <- if (nrow(data)) data$file[order(data$ch, data$z, na.last = TRUE)][1] else NA
    data.frame(base = b, folder = f$folder[1], leaf = f$leaf[1], image_type = f$image_type[1],
               layout = layout, n_files = nrow(data), n_ch_files = n_ch,
               n_z_files = n_z, file_status = status, probe_file = first,
               files = paste(data$file, collapse = ";"), stringsAsFactors = FALSE)
  })
  out <- do.call(rbind, rows)
  # Missing trailing channels can't be seen within one image; compare to the rest
  max_ch <- suppressWarnings(max(out$n_ch_files, na.rm = TRUE))
  short <- out$n_ch_files > 0 & out$n_ch_files < max_ch & out$file_status == "ok"
  out$file_status[short] <- sprintf("fewer channels than other images (%d vs %d)",
                                    out$n_ch_files[short], max_ch)
  out
}
