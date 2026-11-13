# leica_xml.R — read Leica's own acquisition XML (objective, zoom, lasers,
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# detectors, dyes, tiles) straight from the .lif header, or from the MetaData/
# folder that LAS X writes next to TIF exports. Both use the same <Image> schema.
#
# Why: Bio-Formats flattens this XML into key/value pairs and loses most laser
# lines (only one LaserLineSetting per AOTF survives), so it is not reliable for
# laser intensities. This reads the source directly.

xml_num  <- function(node, a) suppressWarnings(as.numeric(xml2::xml_attr(node, a)))
xml_chr  <- function(node, a) { v <- xml2::xml_attr(node, a); if (length(v) == 0) NA_character_ else gsub("\\s+", " ", trimws(v)) }
is_missing_node <- function(n) inherits(n, "xml_missing") || length(n) == 0

# ---- .lif header: int32 magic(0x70), int32 len, byte 0x2A, int32 nChars, UTF-16LE XML
read_lif_xml <- function(path) {
  con <- file(path, "rb"); on.exit(close(con))
  magic <- readBin(con, "integer", size = 4, endian = "little")
  readBin(con, "integer", size = 4, endian = "little")
  test  <- readBin(con, "raw", 1)
  if (!identical(magic, 112L) || !identical(test, as.raw(0x2A)))
    stop("Not a Leica .lif file (unexpected header): ", path, call. = FALSE)
  n_chars <- readBin(con, "integer", size = 4, endian = "little")
  raw <- readBin(con, "raw", n_chars * 2)
  txt <- iconv(list(raw), from = "UTF-16LE", to = "UTF-8")
  xml2::read_xml(txt)
}

# All image elements in document order, with their folder path inside the project
lif_image_table <- function(doc) {
  els <- xml2::xml_find_all(doc, "//Element[Data/Image]")
  rows <- lapply(els, function(el) {
    names <- xml2::xml_attr(xml2::xml_find_all(el, "ancestor-or-self::Element"), "Name")
    names <- names[-1]                                   # drop the project (root) element
    mem <- xml_num(xml2::xml_find_first(el, "./Memory"), "Size")
    data.frame(path = paste(names, collapse = "/"), leaf = utils::tail(names, 1),
               has_data = is.na(mem) || mem > 0, stringsAsFactors = FALSE)
  })
  tab <- do.call(rbind, rows)
  tab$node_index <- seq_along(els)
  list(table = tab[tab$has_data, ], nodes = els)
}

# ---- one <Image> node -> image-level fields + channel table ----------------------
parse_leica_image <- function(image_node) {
  hw <- xml2::xml_find_first(image_node, ".//Attachment[@Name='HardwareSetting']")
  if (is_missing_node(hw)) hw <- image_node
  master <- xml2::xml_find_first(hw, ".//ATLConfocalSettingDefinition[not(ancestor::LDM_Block_Sequential_List)]")
  seqs   <- xml2::xml_find_all(hw, ".//LDM_Block_Sequential_List/ATLConfocalSettingDefinition")

  dims <- xml2::xml_find_all(image_node, "./ImageDescription/Dimensions/DimensionDescription")
  dim_n <- function(id) { d <- dims[xml2::xml_attr(dims, "DimID") == id]; if (length(d)) as.integer(xml2::xml_attr(d[1], "NumberOfElements")) else NA_integer_ }
  dim_step_um <- function(id) {
    d <- dims[xml2::xml_attr(dims, "DimID") == id]; if (!length(d)) return(NA_real_)
    n <- as.integer(xml2::xml_attr(d[1], "NumberOfElements")); len <- xml_num(d[1], "Length")
    unit <- xml2::xml_attr(d[1], "Unit"); scale <- if (identical(unit, "m")) 1e6 else 1
    if (is.na(n) || n < 2) NA_real_ else len * scale / (n - 1)
  }

  ch_nodes <- xml2::xml_find_all(image_node, "./ImageDescription/Channels/ChannelDescription")
  channels <- do.call(rbind, lapply(seq_along(ch_nodes), function(i) {
    ch <- ch_nodes[[i]]
    pr <- xml2::xml_find_all(ch, "./ChannelProperty")
    props <- stats::setNames(xml2::xml_text(xml2::xml_find_first(pr, "./Value")),
                             xml2::xml_text(xml2::xml_find_first(pr, "./Key")))
    seq_i <- suppressWarnings(as.integer(props["SequentialSettingIndex"]))
    setting <- if (!is.na(seq_i) && seq_i + 1 <= length(seqs)) seqs[[seq_i + 1]] else master
    las <- xml2::xml_find_all(setting, ".//LaserLineSetting")
    line <- xml_num(las, "LaserLine"); int <- xml_num(las, "IntensityDev")
    on <- !is.na(int) & int > 0
    det_name <- unname(props["DetectorName"])
    det <- if (!is.na(det_name)) xml2::xml_find_first(setting, sprintf(".//Detector[@Name='%s']", det_name)) else NULL
    data.frame(
      channel_index = i,
      dye_name = sub("^Leica/", "", unname(props["DyeName"])),
      lut_name = xml_chr(ch, "LUTName"),
      sequential_index = seq_i,
      laser_nm = if (any(on)) paste(line[on], collapse = "+") else NA_character_,
      laser_intensity_pct = if (any(on)) paste(round(int[on], 2), collapse = "+") else NA_character_,
      detector = det_name,
      detector_mode = if (is.null(det) || is_missing_node(det)) unname(props["ChannelType"]) else xml_chr(det, "AcquisitionModeName"),
      detector_gain = if (is.null(det) || is_missing_node(det)) NA_real_ else xml_num(det, "Gain"),
      stringsAsFactors = FALSE)
  }))

  image <- data.frame(
    xml_objective_name = xml_chr(master, "ObjectiveName"),
    xml_objective_mag  = xml_num(master, "Magnification"),
    xml_objective_na   = xml_num(master, "NumericalAperture"),
    xml_immersion      = xml_chr(master, "Immersion"),
    zoom               = xml_num(master, "Zoom"),
    scan_speed_hz      = xml_num(master, "ScanSpeed"),
    pinhole_airy       = round(xml_num(master, "PinholeAiry"), 3),
    line_average       = xml_num(master, "LineAverage"),
    frame_average      = xml_num(master, "FrameAverage"),
    line_accumulation  = xml_num(master, "Line_Accumulation"),
    frame_accumulation = xml_num(master, "FrameAccumulation"),
    acq_bit_depth      = xml_num(master, "BitSize"),
    n_sequential       = length(seqs),
    xml_n_tiles        = dim_n("10"),
    xml_size_z         = dim_n("3"),
    xml_pixel_size_um  = dim_step_um("1"),
    xml_z_step_um      = dim_step_um("3"),
    stringsAsFactors = FALSE)

  if (!is.null(channels) && nrow(channels)) {
    lab <- ifelse(is.na(channels$laser_nm), "none",
                  paste0(channels$laser_nm, "nm@", channels$laser_intensity_pct, "%"))
    image$laser_summary <- paste(sprintf("C%d %s", channels$channel_index, lab), collapse = " | ")
    image$detector_summary <- paste(sprintf("C%d %s g%s %s", channels$channel_index, channels$detector,
                                            channels$detector_gain, channels$detector_mode), collapse = " | ")
    image$dyes <- paste(channels$dye_name, collapse = " / ")
  } else image$laser_summary <- image$detector_summary <- image$dyes <- NA_character_
  list(image = image, channels = channels)
}

# ---- match lif XML images to Bio-Formats series ------------------------------------
# 1) exact path ("SLIDE1/Series004"); 2) document order (tiles expanded if Bio-Formats
# split them), verified by name; 3) give up on the ones that can't be matched.
map_xml_to_series <- function(series_names, xml_tab, xml_tiles) {
  idx <- match(series_names, xml_tab$path)
  if (!anyNA(idx) && !anyDuplicated(xml_tab$path)) return(idx)
  for (expand in c(FALSE, TRUE)) {
    reps <- if (expand) pmax(1L, ifelse(is.na(xml_tiles), 1L, xml_tiles)) else rep(1L, nrow(xml_tab))
    ord <- rep(seq_len(nrow(xml_tab)), reps)
    if (length(ord) == length(series_names)) {
      ok <- mapply(function(bf, leaf) grepl(leaf, bf, fixed = TRUE), series_names, xml_tab$leaf[ord])
      if (mean(ok) >= 0.9) return(ord)
    }
  }
  warning("Could not fully match Leica XML to Bio-Formats series; some XML fields will be blank.",
          call. = FALSE)
  idx
}

# ---- entry points used by inventory.R -------------------------------------------------
leica_xml_for_lif <- function(lif_path, series_names) {
  doc <- read_lif_xml(lif_path)
  li  <- lif_image_table(doc)
  parsed <- lapply(li$table$node_index, function(k)
    parse_leica_image(xml2::xml_find_first(li$nodes[[k]], "./Data/Image")))
  tiles <- vapply(parsed, function(p) p$image$xml_n_tiles, integer(1))
  idx <- map_xml_to_series(series_names, li$table, tiles)
  collect_xml(parsed, idx, li$table$path)
}

leica_xml_for_tif <- function(folder, bases) {
  md <- file.path(folder, "MetaData")
  parsed <- lapply(bases, function(b) {
    f <- c(file.path(md, paste0(b, ".xml")), file.path(md, paste0(b, "_Properties.xml")))
    f <- f[file.exists(f)]
    if (!length(f)) return(NULL)
    img <- xml2::xml_find_first(xml2::read_xml(f[1]), "//Image")
    if (is_missing_node(img)) NULL else parse_leica_image(img)
  })
  collect_xml(parsed, seq_along(bases), bases)
}

collect_xml <- function(parsed, idx, paths) {
  template <- parse_template()
  images <- do.call(rbind, lapply(seq_along(idx), function(i) {
    p <- if (is.na(idx[i])) NULL else parsed[[idx[i]]]
    row <- if (is.null(p)) template$image else p$image
    row$xml_found <- !is.null(p); row$xml_path <- if (is.na(idx[i])) NA_character_ else paths[idx[i]]
    row$xml_index <- idx[i]
    row
  }))
  channels <- lapply(seq_along(idx), function(i) {
    p <- if (is.na(idx[i])) NULL else parsed[[idx[i]]]
    if (is.null(p) || is.null(p$channels)) NULL else cbind(row_id = i, p$channels)
  })
  list(images = images, channels = do.call(rbind, channels))
}

parse_template <- function() {
  img <- data.frame(xml_objective_name = NA_character_, xml_objective_mag = NA_real_,
                    xml_objective_na = NA_real_, xml_immersion = NA_character_, zoom = NA_real_,
                    scan_speed_hz = NA_real_, pinhole_airy = NA_real_, line_average = NA_real_,
                    frame_average = NA_real_, line_accumulation = NA_real_, frame_accumulation = NA_real_,
                    acq_bit_depth = NA_real_, n_sequential = NA_integer_, xml_n_tiles = NA_integer_,
                    xml_size_z = NA_integer_, xml_pixel_size_um = NA_real_, xml_z_step_um = NA_real_,
                    laser_summary = NA_character_, detector_summary = NA_character_, dyes = NA_character_,
                    stringsAsFactors = FALSE)
  list(image = img)
}
