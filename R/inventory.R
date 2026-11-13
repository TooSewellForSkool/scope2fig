# inventory.R — Module 1: read the .lif (or TIF folder) and build image_manifest.csv
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# Sources: Bio-Formats (via Fiji) for dimensions/pixel sizes; Leica's own XML
# (lif header or MetaData/ folder) for objective, zoom, lasers, detectors, dyes, tiles.
#
# Outputs in <output_root>/manifest/:
#   image_manifest.csv   one row per image: metadata + grouping + your fields
#   channels.csv         one row per image x channel (dye, laser, %, detector, gain, LUT)
#   groups_auto.csv      auto-detected comparable groups
#   laser_profiles.csv   distinct laser/detector settings, labeled L1, L2, ...
#   tif_file_index.csv   (TIF mode) how every file name was interpreted
#   history/             previous manifests, never overwritten silently
#   raw_inventory/       untouched Fiji output

run_inventory <- function(cfg, pipeline_dir) {
  require_pkgs("xml2")
  raw_dir <- cfg$paths$raw_inventory
  unlink(file.path(raw_dir, "inventory_done.txt"))
  run_fiji_script(
    cfg, file.path(pipeline_dir, "fiji", "inventory.groovy"),
    params = list(input_path = cfg$input$path, out_dir = raw_dir,
                  dump_original_metadata = isTRUE(cfg$inventory$dump_original_metadata),
                  skip_overlay = TRUE,
                  file_extensions = if (cfg$input$mode == "bioformats" && dir.exists(cfg$input$path))
                                      paste(cfg$input$extensions, collapse = ",") else ""),
    log_file = file.path(cfg$paths$logs, sprintf("fiji_inventory_%s.log", timestamp_id())),
    expect_file = file.path(raw_dir, "inventory_done.txt")
  )

  img  <- read_csv_safe(file.path(raw_dir, "inventory_images.csv"))
  chan <- read_csv_safe(file.path(raw_dir, "inventory_channels.csv"))

  bad <- !is.na(img$read_error)
  if (any(bad)) warning("Bio-Formats could not read: ",
                        paste(basename(img$source_file[bad]), collapse = ", "), call. = FALSE)
  img <- img[!bad, , drop = FALSE]

  if (!nrow(img)) stop("No readable images found in ", cfg$input$path, call. = FALSE)
  tables <- switch(cfg$input$mode,
                   lif        = build_from_lif(img, chan, cfg),
                   bioformats = build_from_bioformats(img, chan, cfg),
                   build_from_tif(img, chan, cfg))
  merged <- merge_leica_xml(tables$images, tables$channels, cfg)
  man  <- merged$images
  chan <- merged$channels

  man <- resolve_tiles(man)
  profiles <- assign_laser_profiles(man, chan, cfg$grouping$laser_tolerance_pct %||% 0.5)
  man$laser_profile <- profiles$by_row

  man <- add_derived_fields(man, cfg)
  man <- assign_groups(man, cfg)
  man <- add_user_fields(man, cfg)
  man <- merge_with_existing(man, cfg)
  man <- order_manifest_columns(man, cfg)

  # ---- write
  mdir <- cfg$paths$manifest
  write_csv_safe(man,  file.path(mdir, "image_manifest.csv"))
  write_csv_safe(chan, file.path(mdir, "channels.csv"))
  write_csv_safe(group_summary(man, cfg), file.path(mdir, "groups_auto.csv"))
  write_csv_safe(profiles$table, file.path(mdir, "laser_profiles.csv"))
  if (!is.null(tables$file_index)) write_csv_safe(tables$file_index, file.path(mdir, "tif_file_index.csv"))
  old_keys <- file.path(mdir, "laser_key_summary.csv")
  if (file.exists(old_keys)) file.remove(old_keys)        # superseded by Leica XML

  details <- list(
    n_images = nrow(man), n_included = sum(man$include, na.rm = TRUE),
    n_groups = length(unique(man$group_auto)),
    acq_types = as.list(table(man$acq_type)),
    n_with_acquisition_metadata = sum(man$xml_found, na.rm = TRUE),
    flagged = man$image_uid[man$flags != ""]
  )
  log_path <- write_run_log(cfg, "inventory", details)
  print_inventory_summary(man, profiles, cfg)
  invisible(list(manifest = man, channels = chan, log = log_path))
}

# ---- readers -----------------------------------------------------------------
# ---- lif mode ------------------------------------------------------------------
build_from_lif <- function(img, chan, cfg) {
  lif_base <- tools::file_path_sans_ext(basename(img$source_file))
  img$image_uid  <- sprintf("%s::S%03d", lif_base, img$series_index)
  img$series_name[is.na(img$series_name)] <- sprintf("series_%d", img$series_index[is.na(img$series_name)])
  img$folder     <- ifelse(grepl("/", img$series_name), sub("/[^/]*$", "", img$series_name), "")
  img$leaf_name  <- sub(".*/", "", img$series_name)
  img$name_flat  <- gsub("/", "-", img$series_name)        # how LAS X names exported TIFs
  img$layout <- "lif_series"; img$n_files <- 1L; img$files <- ""; img$file_status <- "ok"

  # Tiles: Bio-Formats may expose each tile as its own series with a repeated name
  same_name <- stats::ave(seq_len(nrow(img)), img$source_file, img$series_name, FUN = length)
  img$n_tiles   <- ifelse(same_name > 1, same_name, NA_integer_)
  img$tile_flag <- same_name > 1 | grepl(cfg$inventory$tile_name_regex, img$series_name, perl = TRUE)

  chan$image_uid <- img$image_uid[match(paste(chan$source_file, chan$series_index),
                                        paste(img$source_file, img$series_index))]
  list(images = img, channels = tidy_channels(chan))
}

# ---- any Bio-Formats file(s): .czi, .nd2, ... (one or more images per file) ----------
build_from_bioformats <- function(img, chan, cfg) {
  fbase <- tools::file_path_sans_ext(basename(img$source_file))
  clash <- tapply(img$source_file, fbase, function(x) length(unique(x)) > 1)   # same name, other extension
  fbase <- ifelse(clash[fbase], basename(img$source_file), fbase)
  img$image_uid <- sprintf("%s::S%03d", fbase, img$series_index)
  n_ser <- stats::ave(seq_len(nrow(img)), img$source_file, FUN = length)
  bf <- ifelse(is.na(img$series_name), paste0("S", img$series_index), img$series_name)
  img$series_name <- ifelse(n_ser == 1, fbase, paste0(fbase, " - ", bf))
  img$folder <- ""; img$leaf_name <- img$series_name
  img$name_flat <- gsub("[/\\\\:]+", "-", img$series_name)
  img$layout <- "bioformats_series"; img$n_files <- 1L; img$files <- basename(img$source_file)
  img$file_status <- "ok"; img$n_tiles <- NA_integer_
  img$tile_flag <- grepl(cfg$inventory$tile_name_regex, img$series_name, perl = TRUE)
  # pyramids (slide scanners): Bio-Formats lists each lower resolution as its own series
  prev_x <- c(NA, img$size_x[-nrow(img)]); same_file <- c(FALSE, img$source_file[-1] == img$source_file[-nrow(img)])
  img$pyramid_level <- same_file & !is.na(prev_x) & abs(img$size_x * 2 - prev_x) <= 2
  chan$image_uid <- img$image_uid[match(paste(chan$source_file, chan$series_index),
                                        paste(img$source_file, img$series_index))]
  list(images = img, channels = tidy_channels(chan))
}

# ---- tif fallback mode -----------------------------------------------------------
build_from_tif <- function(probe, chan, cfg) {
  files  <- list.files(cfg$input$path, pattern = "\\.tiff?$", ignore.case = TRUE)
  parsed <- parse_leica_tif_names(files)
  if (any(parsed$role == "unrecognized"))
    warning("Unrecognized file names: ", paste(parsed$file[parsed$role == "unrecognized"], collapse = ", "),
            call. = FALSE)
  s <- summarise_tif_images(parsed)
  p <- probe[match(s$probe_file, basename(probe$source_file)), ]

  img <- data.frame(
    image_uid = paste0("tif::", s$base), source_file = cfg$input$path, source_mode = "tif_folder",
    series_index = NA_integer_, series_name = s$base, folder = s$folder, leaf_name = s$leaf,
    name_flat = s$base,
    size_x = p$size_x, size_y = p$size_y,
    size_z = ifelse(s$layout == "split_channels_z", s$n_z_files, p$size_z),
    size_c = ifelse(s$layout %in% c("split_channels", "split_channels_z"), s$n_ch_files, p$size_c),
    size_t = p$size_t, pixel_type = p$pixel_type, bits_per_pixel = p$bits_per_pixel,
    is_rgb = p$is_rgb, pixel_size_x_um = p$pixel_size_x_um, pixel_size_y_um = p$pixel_size_y_um,
    z_step_um = p$z_step_um, acquisition_date = p$acquisition_date,
    objective_model = p$objective_model, objective_mag = p$objective_mag,
    objective_na = p$objective_na, objective_immersion = p$objective_immersion,
    stage_x = p$stage_x, stage_y = p$stage_y, stage_z = p$stage_z,
    layout = s$layout, n_files = s$n_files, files = s$files, file_status = s$file_status,
    n_tiles = NA_integer_, stringsAsFactors = FALSE
  )
  img$tile_flag <- grepl(cfg$inventory$tile_name_regex, img$series_name, perl = TRUE)

  # Channel table: one row per channel file (split) or per channel in the single file
  ch_rows <- lapply(seq_len(nrow(s)), function(i) {
    uid <- img$image_uid[i]
    if (s$layout[i] %in% c("split_channels", "split_channels_z")) {
      ids <- sort(unique(parsed$ch[parsed$base == s$base[i] & parsed$role == "channel"]))
      data.frame(image_uid = uid, channel_index = ids + 1L, channel_name = sprintf("ch%02d", ids),
                 stringsAsFactors = FALSE)
    } else if (s$layout[i] == "single_file") {
      cc <- chan[basename(chan$source_file) == s$probe_file[i], ]
      if (nrow(cc)) data.frame(image_uid = uid, channel_index = cc$channel_index + 1L,
                               channel_name = cc$channel_name, stringsAsFactors = FALSE)
    }
  })
  ch <- do.call(rbind, ch_rows)
  if (is.null(ch)) ch <- data.frame(image_uid = character(), channel_index = integer(),
                                    channel_name = character())
  for (col in c("excitation_nm", "emission_nm", "ome_attenuation", "detector_gain", "lif_color"))
    ch[[col]] <- rep(NA, nrow(ch))
  list(images = img, channels = ch, file_index = parsed)
}

tidy_channels <- function(chan) {
  chan$channel_index <- chan$channel_index + 1L          # 1-based, like Fiji's C1..C4
  cols <- c("image_uid", "channel_index", "channel_name", "excitation_nm", "emission_nm",
            "ome_attenuation", "detector_gain", "lif_color", "exposure_ms", "fluor", "acquisition_mode")
  chan[, intersect(cols, names(chan))]
}

# ---- Leica XML: objective, zoom, lasers, detectors, tiles -----------------------------
coalesce_num <- function(a, b) ifelse(is.na(a), b, a)

merge_leica_xml <- function(man, chan, cfg) {
  xml <- tryCatch(switch(cfg$input$mode,
      lif        = leica_xml_for_lif(cfg$input$path, man$series_name),
      bioformats = vendor_meta_for_bioformats(man, chan),
      leica_xml_for_tif(cfg$input$path, man$series_name)),
    error = function(e) { warning("Acquisition metadata not read: ", conditionMessage(e), call. = FALSE); NULL })
  if (is.null(xml)) xml <- list(images = parse_template()$image[rep(1, nrow(man)), ], channels = NULL)
  if (is.null(xml$images$xml_found)) {
    xml$images$xml_found <- FALSE; xml$images$xml_path <- NA_character_; xml$images$xml_index <- NA_integer_ }
  xi <- xml$images; rownames(xi) <- NULL
  man <- cbind(man, xi)

  # Leica XML is the authority for acquisition settings; Bio-Formats fills gaps
  man$objective_model     <- ifelse(is.na(man$xml_objective_name), man$objective_model, man$xml_objective_name)
  man$objective_mag       <- coalesce_num(man$xml_objective_mag, man$objective_mag)
  man$objective_na        <- coalesce_num(man$xml_objective_na, man$objective_na)
  man$objective_immersion <- ifelse(is.na(man$xml_immersion), man$objective_immersion, man$xml_immersion)
  man$pixel_size_x_um     <- coalesce_num(man$pixel_size_x_um, man$xml_pixel_size_um)
  man$z_step_um           <- coalesce_num(man$z_step_um, man$xml_z_step_um)
  has_tiles <- !is.na(man$xml_n_tiles) & man$xml_n_tiles > 1
  man$n_tiles[has_tiles]   <- man$xml_n_tiles[has_tiles]
  man$tile_flag            <- man$tile_flag | has_tiles
  man$xml_objective_name <- man$xml_objective_mag <- man$xml_objective_na <- man$xml_immersion <- NULL

  if (!is.null(xml$channels) && nrow(xml$channels)) {
    xc <- xml$channels
    xc$image_uid <- man$image_uid[xc$row_id]; xc$row_id <- NULL
    keep <- setdiff(names(chan), setdiff(names(xc), c("image_uid", "channel_index")))   # vendor values win
    chan <- merge(chan[, keep], xc, by = c("image_uid", "channel_index"), all = TRUE, sort = FALSE)
    chan <- chan[order(match(chan$image_uid, man$image_uid), chan$channel_index), ]
  }
  list(images = man, channels = chan)
}

# Same lasers/detectors/gains and intensities within `tol` percentage points = one
# profile (LAS X stores e.g. 10% as 10.2%, which is not a real settings change).
assign_laser_profiles <- function(man, chan, tol) {
  prof <- rep(NA_character_, nrow(man)); refs <- list()
  for (i in seq_len(nrow(man))) {
    if (is.na(man$laser_summary[i]) || is.null(chan$laser_nm)) next
    ch   <- chan[chan$image_uid == man$image_uid[i], ]
    key  <- paste(ch$laser_nm, ch$detector, ch$detector_gain, ch$detector_mode,
                  if (!is.null(ch$exposure_ms)) signif(ch$exposure_ms, 6), collapse = " | ")   # exposure: widefield
    vals <- suppressWarnings(as.numeric(unlist(strsplit(
              ifelse(is.na(ch$laser_intensity_pct), "0", ch$laser_intensity_pct), "+", fixed = TRUE))))
    hit <- 0
    for (k in seq_along(refs)) {
      r <- refs[[k]]
      if (r$key == key && length(r$vals) == length(vals) && max(abs(r$vals - vals)) <= tol) { hit <- k; break }
    }
    if (!hit) { refs[[length(refs) + 1]] <- list(key = key, vals = vals, rows = integer()); hit <- length(refs) }
    refs[[hit]]$rows <- c(refs[[hit]]$rows, i)
    prof[i] <- paste0("L", hit)
  }
  tab <- if (length(refs)) do.call(rbind, lapply(seq_along(refs), function(k) {
    rows <- refs[[k]]$rows
    data.frame(laser_profile = paste0("L", k), n_images = length(rows),
               lasers = paste(unique(man$laser_summary[rows]), collapse = "  //  "),
               detectors = man$detector_summary[rows[1]], stringsAsFactors = FALSE)
  })) else data.frame(laser_profile = character(), n_images = integer(),
                      lasers = character(), detectors = character())
  list(by_row = prof, table = tab)
}

# ---- tile scans ------------------------------------------------------------------
# Bio-Formats exposes each tile of a LAS X tile scan as its own series; the stitched
# ("Merged") image is a separate series. Tiles get grouped under their scan and are
# excluded by default when a stitched version exists (you can switch them back on).
resolve_tiles <- function(man) {
  n <- nrow(man)
  set_id   <- ifelse(!is.na(man$xml_path), man$xml_path, man$series_name)
  set_size <- stats::ave(seq_len(n), set_id, FUN = length)
  is_tile  <- man$tile_flag & ((!is.na(man$n_tiles) & man$n_tiles > 1) | set_size > 1)
  man$tile_role  <- ifelse(is_tile, "tile", ifelse(man$tile_flag, "stitched", NA_character_))
  man$tile_set   <- ifelse(is_tile, set_id, NA_character_)
  man$tile_index <- ifelse(is_tile, stats::ave(seq_len(n), set_id, FUN = seq_along), NA_integer_)
  man$n_tiles[is_tile] <- set_size[is_tile]
  man$tile_parent <- NA_character_
  sets <- unique(man$tile_set[is_tile])
  for (s in sets) {
    rows <- which(man$tile_set %in% s)
    fld  <- man$folder[rows[1]]; leaf <- tolower(man$leaf_name[rows[1]])
    cand <- which(man$tile_role %in% "stitched" & man$folder == fld)
    pick <- cand[startsWith(tolower(man$leaf_name[cand]), leaf)]
    # one tile set + one stitched image in the same folder -> pair them
    if (!length(pick) && length(cand) == 1 &&
        sum(vapply(sets, function(x) man$folder[which(man$tile_set %in% x)[1]] == fld, logical(1))) == 1)
      pick <- cand
    if (length(pick)) man$tile_parent[rows] <- man$image_uid[pick[1]]
  }
  man$tile_note <- ifelse(is_tile & !is.na(man$tile_parent),
    sprintf("tile %s/%s; excluded by default (stitched: %s)", man$tile_index, man$n_tiles, man$tile_parent),
    ifelse(is_tile, sprintf("tile %s/%s; no stitched version found, tiles kept", man$tile_index, man$n_tiles),
           NA_character_))
  pyr <- if (is.null(man$pyramid_level)) rep(FALSE, n) else man$pyramid_level %in% TRUE
  man$include_auto <- !(man$layout %in% "overlay_only") & !(is_tile & !is.na(man$tile_parent)) & !pyr
  man
}

# ---- derived columns, grouping, user fields ------------------------------------------
add_derived_fields <- function(man, cfg) {
  man$pixel_size_um <- man$pixel_size_x_um
  man$z_range_um    <- ifelse(man$size_z > 1 & !is.na(man$z_step_um), (man$size_z - 1) * man$z_step_um, NA)
  man$processed_flag <- grepl(cfg$inventory$processed_name_regex, man$series_name, perl = TRUE)
  man$name_is_default <- grepl(cfg$naming$default_name_regex, man$leaf_name, perl = TRUE)
  tp <- regex_named(man$leaf_name, "(?i)^(?<type>Series|Image|Region|Position|TileScan|Mark_and_Find)")
  man$image_type <- ifelse(is.na(tp$type), "custom", tools::toTitleCase(tolower(tp$type)))
  man$acq_type <- ifelse(man$tile_flag & man$size_z > 1, "tile_scan_z",
                  ifelse(man$tile_flag, "tile_scan",
                  ifelse(!is.na(man$size_t) & man$size_t > 1, "time_series",
                  ifelse(man$size_z > 1, "z_stack", "single_plane"))))
  man$acq_type[is.na(man$acq_type)] <- "unknown"

  flags <- character(nrow(man))
  add <- function(cond, txt) {
    cond[is.na(cond)] <- FALSE; txt <- rep_len(txt, length(cond))
    flags[cond] <<- paste0(flags[cond], txt[cond], "; ")
  }
  add(man$file_status != "ok", man$file_status)
  add(man$processed_flag, "looks processed (Lightning/deconv)")
  add(!is.na(man$tile_note), man$tile_note)
  add(is.na(man$pixel_size_um), "no pixel size")
  add(is.na(man$objective_mag), "no objective info")
  add(!man$xml_found, "no acquisition metadata (lasers/exposure/detectors unknown)")
  if (!is.null(man$pyramid_level)) add(man$pyramid_level, "lower-resolution pyramid copy - excluded")
  man$flags <- sub("; $", "", flags)
  man
}

assign_groups <- function(man, cfg) {
  fields <- intersect(cfg$grouping$key_fields, names(man))
  fmt <- function(f) {
    v <- man[[f]]
    if (f %in% c("pixel_size_um", "z_step_um")) v <- signif(as.numeric(v), cfg$grouping$pixel_size_sig_digits)
    paste0(f, "=", ifelse(is.na(v), "NA", as.character(v)))
  }
  man$comparability_key <- do.call(paste, c(lapply(fields, fmt), sep = " | "))
  keys <- unique(man$comparability_key)
  idx  <- match(man$comparability_key, keys)
  mag  <- ifelse(is.na(man$objective_mag), "objNA", paste0(as.character(man$objective_mag), "x"))
  man$group_auto <- sprintf("G%02d_%s_%s_%sx%s", idx, man$acq_type, mag, man$size_x, man$size_y)
  # differing laser settings inside one group is worth a warning, not a split
  lp <- tapply(man$laser_profile, man$group_auto, function(v) length(unique(stats::na.omit(v))))
  diff_grp <- names(lp)[lp > 1]
  hit <- man$group_auto %in% diff_grp
  man$flags[hit] <- ifelse(man$flags[hit] == "", "laser settings differ within group",
                           paste0(man$flags[hit], "; laser settings differ within group"))
  man
}

add_user_fields <- function(man, cfg) {
  uf <- unique(unlist(cfg$user_fields))
  for (f in uf) man[[f]] <- NA_character_
  man$user_fields_source <- "blank"
  got <- rep(FALSE, nrow(man))
  # 1) built-in/configurable patterns per field (naming.name_patterns), searched in the full
  #    name incl. LAS X folders ("SLIDE3-2/Series004") - only fills what is found
  if (!isFALSE(cfg$naming$parse_names) && length(cfg$naming$name_patterns)) {
    pf <- parse_name_fields(man$series_name, cfg$naming$name_patterns, uf)
    for (f in uf) { hit <- !is.na(pf[[f]]); man[[f]][hit] <- pf[[f]][hit]; got <- got | hit }
  }
  # 2) optional single regex with named groups (naming.name_fields_regex) overrides
  rx <- cfg$naming$name_fields_regex %||% ""
  if (nzchar(rx)) {
    parsed <- regex_named(man$leaf_name, rx)
    for (f in intersect(names(parsed), uf)) { hit <- !is.na(parsed[[f]]); man[[f]][hit] <- parsed[[f]][hit]; got <- got | hit }
  }
  man$user_fields_source[got] <- "parsed_from_name"
  man$include <- man$include_auto
  man$group <- man$group_auto
  # experiment = staining panel; owns channel names/colors (session > experiment > group)
  panel <- ifelse(is.na(man$dyes), paste0(man$size_c, " channels"), man$dyes)
  man$experiment_auto <- paste0("E", match(panel, unique(panel)))
  man$experiment <- man$experiment_auto
  man$session <- cfg$session_name
  man$needs_review <- TRUE
  man
}

# Keep everything the user already confirmed/edited when re-running
merge_with_existing <- function(man, cfg) {
  path <- file.path(cfg$paths$manifest, "image_manifest.csv")
  if (!file.exists(path)) return(man)
  old <- read_csv_safe(path)
  file.copy(path, file.path(cfg$paths$history, sprintf("image_manifest_%s.csv", timestamp_id())))
  idx <- match(man$image_uid, old$image_uid); has <- !is.na(idx)
  if ("needs_review" %in% names(old)) man$needs_review[has] <- old$needs_review[idx[has]]
  # your fields: keep what you typed or imported; values that were only pre-filled from the
  # name (or blank) are refreshed from the current name patterns
  if ("user_fields_source" %in% names(old)) {
    mine <- has
    mine[has] <- old$user_fields_source[idx[has]] %in% c("user", "imported")
    for (k in intersect(unlist(cfg$user_fields), names(old))) man[[k]][mine] <- old[[k]][idx[mine]]
    man$user_fields_source[mine] <- old$user_fields_source[idx[mine]]
  }
  # include: keep the old value only if the user changed it from the automatic default
  if ("include" %in% names(old)) {
    old_auto <- if ("include_auto" %in% names(old)) as.logical(old$include_auto) else rep(NA, nrow(old))
    edited <- has & !is.na(old_auto[idx]) & as.logical(old$include[idx]) != old_auto[idx]
    edited[is.na(edited)] <- FALSE
    man$include[edited] <- as.logical(old$include[idx[edited]])
  }
  # group / experiment: keep the old value only if the user changed it from the auto value
  for (k in c("group", "experiment")) {
    ka <- paste0(k, "_auto")
    if (all(c(k, ka) %in% names(old))) {
      edited <- has & !is.na(old[[k]][idx]) & old[[k]][idx] != old[[ka]][idx]
      edited[is.na(edited)] <- FALSE
      man[[k]][edited] <- old[[k]][idx[edited]]
    }
  }
  renamed <- has & old$series_name[idx] != man$series_name
  if (any(renamed, na.rm = TRUE))
    warning("Series name changed for: ", paste(man$image_uid[which(renamed)], collapse = ", "),
            " - check the .lif was not re-saved with a different order.", call. = FALSE)
  gone <- setdiff(old$image_uid, man$image_uid)
  if (length(gone)) {
    warning(length(gone), " image(s) from the previous manifest are no longer in the input; ",
            "see manifest/removed_images.csv", call. = FALSE)
    write_csv_safe(old[old$image_uid %in% gone, ], file.path(cfg$paths$manifest, "removed_images.csv"))
  }
  msg("Kept your previous edits for ", sum(has), " image(s); ", sum(!has), " new image(s).")
  man
}

order_manifest_columns <- function(man, cfg) {
  first <- c("image_uid", "include", "include_auto", "session", "experiment", "experiment_auto",
             "group", "group_auto", unlist(cfg$user_fields),
             "user_fields_source", "needs_review", "flags",
             "series_name", "folder", "leaf_name", "name_flat", "image_type", "name_is_default",
             "acq_type", "size_x", "size_y", "size_z", "size_c", "size_t",
             "bits_per_pixel", "pixel_type", "pixel_size_um", "z_step_um", "z_range_um",
             "n_tiles", "tile_flag", "tile_role", "tile_index", "tile_set", "tile_parent", "processed_flag",
             "objective_model", "objective_mag", "objective_na", "objective_immersion", "zoom",
             "laser_profile", "laser_summary", "detector_summary", "dyes",
             "acq_bit_depth", "scan_speed_hz", "pinhole_airy", "line_average", "frame_average",
             "line_accumulation", "frame_accumulation", "n_sequential",
             "acquisition_date", "comparability_key", "xml_found", "xml_index",
             "source_mode", "source_file", "series_index", "layout", "n_files", "files", "file_status")
  first <- intersect(unique(first), names(man))
  man[, c(first, setdiff(names(man), first))]
}

group_summary <- function(man, cfg) {
  suppressWarnings(do.call(rbind, lapply(split(man, man$group_auto), function(d) data.frame(
    group_auto = d$group_auto[1], n_images = nrow(d), n_included = sum(d$include, na.rm = TRUE),
    acq_type = d$acq_type[1], objective = paste(unique(d$objective_mag), collapse = "/"),
    size = paste0(d$size_x[1], "x", d$size_y[1]), pixel_size_um = d$pixel_size_um[1],
    z_slices = paste(unique(range(d$size_z, na.rm = TRUE)), collapse = "-"),
    laser_profiles = paste(sort(unique(stats::na.omit(d$laser_profile))), collapse = ","),
    comparability_key = d$comparability_key[1], stringsAsFactors = FALSE))))
}

print_inventory_summary <- function(man, profiles, cfg) {
  cat("\n================ Inventory summary ================\n")
  cat("Input:  ", cfg$input$path, " (", cfg$input$mode, ")\n", sep = "")
  cat("Output: ", cfg$paths$manifest, "\n", sep = "")
  cat("Images: ", nrow(man), " (", sum(man$include), " included)\n\n", sep = "")
  gs <- table(man$group_auto); gi <- tapply(man$include, man$group_auto, sum)
  cat("Auto groups (comparable images):\n")
  for (g in names(gs)) cat(sprintf("  %-40s %3d image(s), %3d included\n", g, gs[[g]], gi[[g]]))
  ts <- unique(man$tile_set[!is.na(man$tile_set)])
  if (length(ts)) {
    cat("\nTile scans:\n")
    for (s in ts) {
      r <- man[man$tile_set %in% s, ]
      cat(sprintf("  %s: %d tiles -> %s\n", s, nrow(r),
                  if (is.na(r$tile_parent[1])) "no stitched image found (tiles kept)"
                  else paste("stitched image", r$tile_parent[1], "is used; tiles excluded")))
    }
  }
  fl <- man[man$flags != "" & man$include, c("image_uid", "flags")]
  if (nrow(fl)) {
    cat("\nFlagged for review (included images):\n")
    for (i in seq_len(min(nrow(fl), 15))) cat("  ", fl$image_uid[i], ": ", fl$flags[i], "\n", sep = "")
    if (nrow(fl) > 15) cat("  ... and", nrow(fl) - 15, "more (see image_manifest.csv)\n")
  }
  cat("\nLaser/detector profiles (laser_profiles.csv):\n")
  for (i in seq_len(nrow(profiles$table)))
    cat(sprintf("  %-4s %3d image(s)  %s\n", profiles$table$laser_profile[i],
                profiles$table$n_images[i], profiles$table$lasers[i]))
  cat("===================================================\n\n")
}
