# vendor_meta.R — acquisition metadata for non-Leica files (input.mode = "bioformats")
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
#   .czi (Zeiss)  read straight from the file's own XML: objective, microscope, per-channel
#                 fluor, excitation/emission, exposure, filter cube, acquisition mode
#                 (WideField / StructuredIllumination = Apotome / LaserScanningConfocal),
#                 laser lines + power where present (LSM), camera/detector, display color
#   anything else (.nd2, .oir, ...) uses the standard OME fields Bio-Formats provides
#                 (channel name/fluor, excitation, exposure, gain, color). A Nikon-specific
#                 reader can be added here once we have .nd2 files to test against.
# Output has the same shape as the Leica XML reader (R/leica_xml.R), so the rest of the
# pipeline treats every format alike. "laser_summary" holds whatever defines comparable
# acquisition: laser % for confocals, exposure times for widefield/Apotome.

# ---- Zeiss .czi -------------------------------------------------------------------
read_czi_xml <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  i32 <- function() readBin(con, "integer", size = 4, endian = "little")
  i64 <- function() { lo <- u32(i32()); hi <- u32(i32()); lo + hi * 2^32 }
  sid <- function() { r <- readBin(con, "raw", 16); rawToChar(r[r != as.raw(0)]) }
  if (sid() != "ZISRAWFILE") stop("not a Zeiss .czi file")
  seek(con, 32 + 52); i64(); meta_pos <- i64()            # DirectoryPosition, MetadataPosition
  seek(con, meta_pos)
  if (sid() != "ZISRAWMETADATA") stop("CZI metadata segment not found")
  seek(con, meta_pos + 32); xml_size <- i32()
  seek(con, meta_pos + 32 + 256)
  txt <- rawToChar(readBin(con, "raw", xml_size)); Encoding(txt) <- "UTF-8"
  xml2::read_xml(txt)
}

parse_czi_meta <- function(doc) {
  txt <- function(node, xp) {
    n <- xml2::xml_find_first(node, xp)
    if (inherits(n, "xml_missing")) NA_character_ else { v <- trimws(xml2::xml_text(n)); if (nzchar(v)) v else NA_character_ }
  }
  num <- function(node, xp) suppressWarnings(as.numeric(txt(node, xp)))
  obj <- xml2::xml_find_first(doc, "//Information/Instrument/Objectives/Objective")
  sx  <- num(doc, "//Scaling/Items/Distance[@Id='X']/Value")
  sz  <- num(doc, "//Scaling/Items/Distance[@Id='Z']/Value")
  cam <- txt(doc, "//Information/Instrument/Detectors/Detector/@Name")

  chs <- xml2::xml_find_all(doc, "//Information/Image/Dimensions/Channels/Channel")
  channels <- do.call(rbind, lapply(seq_along(chs), function(i) {
    ch <- chs[[i]]
    col <- txt(ch, "./Color")                                  # Zeiss: #AARRGGBB
    col <- if (!is.na(col) && grepl("^#[0-9A-Fa-f]{8}$", col)) paste0("#", substr(col, 4, 9)) else col
    ls <- xml2::xml_find_all(ch, ".//LightSourceSettings")
    wl <- suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(ls, "./Wavelength"))))
    tr <- suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(ls, "./Transmission"))))
    at <- suppressWarnings(as.numeric(xml2::xml_text(xml2::xml_find_first(ls, "./Attenuation"))))
    pct <- ifelse(!is.na(tr), tr * 100, ifelse(!is.na(at), (1 - at) * 100, NA))
    on <- !is.na(wl)
    det <- sub("^Detector:", "", txt(ch, "./DetectorSettings/Detector/@Id"))
    data.frame(
      channel_index = i,
      dye_name = txt(ch, "./Fluor") %||% xml2::xml_attr(ch, "Name"),
      lut_name = col, sequential_index = NA_integer_,
      laser_nm = if (any(on)) paste(round(wl[on]), collapse = "+") else NA_character_,
      laser_intensity_pct = if (any(on & !is.na(pct))) paste(round(pct[on & !is.na(pct)], 2), collapse = "+") else NA_character_,
      detector = if (is.na(det) || !nzchar(det)) cam else det,
      detector_mode = txt(ch, "./AcquisitionMode"),
      detector_gain = num(ch, "./DetectorSettings/Gain"),
      exposure_ms = num(ch, "./ExposureTime") / 1e6,           # stored in nanoseconds
      filter = txt(ch, "./Reflector"),
      excitation_nm = num(ch, "./ExcitationWavelength"),
      stringsAsFactors = FALSE)
  }))

  image <- parse_template()$image
  image$xml_objective_name <- xml2::xml_attr(obj, "Name")
  image$xml_objective_mag  <- num(obj, "./NominalMagnification")
  image$xml_objective_na   <- num(obj, "./LensNA")
  image$xml_immersion      <- txt(obj, "./Immersion")
  image$acq_bit_depth      <- num(doc, "//Information/Image/ComponentBitCount")
  image$xml_pixel_size_um  <- sx * 1e6
  image$xml_z_step_um      <- sz * 1e6
  summarise_channels_into(image, channels)
}

# ---- any format: standard OME fields from the Bio-Formats inventory -----------------
generic_meta_from_channels <- function(ch) {
  if (is.null(ch) || !nrow(ch)) return(NULL)
  ch <- ch[order(ch$channel_index), ]
  pick <- function(col) if (!is.null(ch[[col]])) ch[[col]] else rep(NA, nrow(ch))
  channels <- data.frame(
    channel_index = ch$channel_index,
    dye_name = ifelse(is.na(pick("fluor")), pick("channel_name"), pick("fluor")),
    lut_name = pick("lif_color"), sequential_index = NA_integer_,
    laser_nm = as.character(round(as.numeric(pick("excitation_nm")))),
    laser_intensity_pct = NA_character_, detector = NA_character_,
    detector_mode = pick("acquisition_mode"), detector_gain = as.numeric(pick("detector_gain")),
    exposure_ms = as.numeric(pick("exposure_ms")), filter = NA_character_,
    excitation_nm = as.numeric(pick("excitation_nm")), stringsAsFactors = FALSE)
  summarise_channels_into(parse_template()$image, channels)
}

# Human-readable summaries; exposure stands in for laser % on widefield systems
summarise_channels_into <- function(image, channels) {
  if (!is.null(channels) && nrow(channels)) {
    one <- function(i) {
      cc <- channels[i, ]; bits <- character()
      if (!is.na(cc$laser_nm) && !is.na(cc$laser_intensity_pct)) bits <- c(bits, paste0(cc$laser_nm, "nm@", cc$laser_intensity_pct, "%"))
      else if (!is.na(cc$excitation_nm)) bits <- c(bits, paste0("ex", round(cc$excitation_nm)))
      if (!is.na(cc$exposure_ms)) bits <- c(bits, paste0("exp ", signif(cc$exposure_ms, 4), " ms"))
      if (!is.na(cc$filter)) bits <- c(bits, paste0("[", cc$filter, "]"))
      if (!length(bits)) "?" else paste(bits, collapse = " ")
    }
    s <- vapply(seq_len(nrow(channels)), one, "")
    image$laser_summary <- if (all(s == "?")) NA_character_ else paste(sprintf("C%d %s", channels$channel_index, s), collapse = " | ")
    det <- sprintf("C%d %s%s%s", channels$channel_index, ifelse(is.na(channels$detector), "", channels$detector),
                   ifelse(is.na(channels$detector_gain), "", paste0(" g", channels$detector_gain)),
                   ifelse(is.na(channels$detector_mode), "", paste0(" ", channels$detector_mode)))
    image$detector_summary <- paste(det, collapse = " | ")
    image$dyes <- paste(channels$dye_name, collapse = " / ")
  }
  list(image = image, channels = channels)
}

# Entry point used by inventory.R for input.mode = "bioformats"
vendor_meta_for_bioformats <- function(man, chan) {
  czi_cache <- list()
  parsed <- lapply(seq_len(nrow(man)), function(i) {
    f <- man$source_file[i]
    if (grepl("\\.czi$", f, ignore.case = TRUE)) {
      if (is.null(czi_cache[[f]]))
        czi_cache[[f]] <<- tryCatch(parse_czi_meta(read_czi_xml(f)), error = function(e) {
          warning("Zeiss metadata not read from ", basename(f), ": ", conditionMessage(e), call. = FALSE); FALSE })
      if (!isFALSE(czi_cache[[f]])) return(czi_cache[[f]])
    }
    generic_meta_from_channels(chan[chan$image_uid == man$image_uid[i], ])
  })
  collect_xml(parsed, seq_along(parsed), rep(NA_character_, length(parsed)))
}
