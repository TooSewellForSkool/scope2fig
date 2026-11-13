# tune.R — Module 3: interactive brightness & contrast (B&C) per experiment / group
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# Settings are LINEAR only (min/max display range per channel; no gamma) and apply to
# every image in the group. min/max are raw intensity values (e.g. photon counts) of
# the max projection, exactly what Fiji's setMinAndMax() will use when rendering.
#
# Writes <output_root>/settings/:
#   <experiment>__<group>.json   one file per group (what the render step reads)
#   bc_settings_summary.csv      all groups at a glance
# and logs/tune_*.json

settings_dir <- function(cfg) file.path(cfg$paths$root, "settings")
unit_key  <- function(experiment, group) paste(experiment, group, sep = " / ")
unit_file <- function(cfg, experiment, group)
  file.path(settings_dir(cfg), paste0(safe_uid(experiment), "__", safe_uid(group), ".json"))

run_tune <- function(cfg, pipeline_dir, launch = TRUE) {
  require_pkgs(c("shiny", "tiff", "png", "jsonlite"))
  mpath <- file.path(cfg$paths$manifest, "image_manifest.csv")
  if (!file.exists(mpath)) stop("No manifest yet - run 'inventory' and 'review' first.", call. = FALSE)
  man <- prepare_manifest_for_review(read_csv_safe(mpath), cfg)
  if (any(man$needs_review %in% TRUE))
    warning("The review step hasn't been saved yet - groups and channel names may not be final.", call. = FALSE)
  chan <- read_csv_safe(file.path(cfg$paths$manifest, "channels.csv"))
  cmap <- build_channel_map(man, chan, file.path(cfg$paths$manifest, "channel_map.csv"))
  inc  <- man[man$include %in% TRUE, ]
  if (!nrow(inc)) stop("No included images - check the review step.", call. = FALSE)

  pp <- run_previews(cfg, pipeline_dir, inc, cmap)                 # only makes missing ones
  stats <- read_csv_safe(pp$stats)
  dir.create(settings_dir(cfg), recursive = TRUE, showWarnings = FALSE)
  units <- build_units(cfg, inc, cmap, stats)
  for (u in units) {
    if (u$saved) msg(sprintf("Loaded saved settings for %s (saved %s): max = %s", u$key, u$saved_at,
                             paste(u$settings$label, u$settings$max, sep = " ", collapse = ", ")))
    else msg("No saved settings yet for ", u$key, " - it will start with automatic values.")
  }
  if (!launch) return(invisible(tune_unattended(cfg, units)))
  app <- tune_app(cfg, inc, units, pp)
  msg("Opening the B&C app in your browser. Use 'Save & close' when done.")
  invisible(shiny::runApp(app, launch.browser = TRUE))
}

# One entry per experiment/group: images, per-channel ranges, current settings
build_units <- function(cfg, inc, cmap, stats) {
  keys <- unique(unit_key(inc$experiment, inc$group))
  units <- lapply(keys, function(k) {
    rows <- inc[unit_key(inc$experiment, inc$group) == k, ]
    e <- rows$experiment[1]; g <- rows$group[1]
    n_ch <- max(rows$size_c, na.rm = TRUE)
    cm <- cmap[cmap$experiment == e, ]; cm <- cm[order(cm$channel_index), ]
    st <- stats[stats$uid_safe %in% safe_uid(rows$image_uid), ]
    ch <- lapply(seq_len(n_ch), function(i) {
      s <- st[st$channel_index == i, ]
      rmax <- if (nrow(s)) max(s$max, na.rm = TRUE) else 255
      p999 <- if (nrow(s)) stats::median(s$p999, na.rm = TRUE) else rmax
      data.frame(channel_index = i,
                 label = if (i <= nrow(cm)) cm$label[i] else paste0("C", i),
                 dye_name = if (i <= nrow(cm)) cm$dye_name[i] else NA_character_,
                 color = if (i <= nrow(cm)) cm$color[i] else rep_len(DEFAULT_COLORS, i)[i],
                 range_max = max(1, ceiling(rmax)), auto_max = max(1, p999),
                 stringsAsFactors = FALSE)
    })
    ch <- do.call(rbind, ch)
    defaults <- data.frame(channel_index = ch$channel_index, label = ch$label, dye_name = ch$dye_name,
                           color = ch$color, min = 0, max = round_step(ch$auto_max, ch$range_max),
                           in_merge = TRUE, stringsAsFactors = FALSE)
    f <- unit_file(cfg, e, g)
    saved <- if (file.exists(f)) read_unit_settings(f) else NULL
    saved_at <- if (file.exists(f)) tryCatch({ j <- jsonlite::read_json(f)
      paste0(j$saved, if (!is.null(j$source) && j$source != "B&C app") paste0(", ", j$source) else "") },
      error = function(e) NA) else NA
    current <- if (!is.null(saved)) apply_settings(defaults, saved) else defaults
    ch$range_max <- pmax(ch$range_max, ceiling(current$max))
    list(key = k, experiment = e, group = g, uids = rows$image_uid, channels = ch,
         defaults = defaults, settings = current, file = f, saved = !is.null(saved), saved_at = saved_at)
  })
  stats::setNames(units, keys)
}

slider_step <- function(range_max) ifelse(range_max <= 20, 0.1, ifelse(range_max <= 300, 1, round(range_max / 300)))
round_step  <- function(x, range_max) { s <- slider_step(range_max); pmax(s, ceiling(x / s) * s) }

read_unit_settings <- function(path) {
  j <- jsonlite::read_json(path, simplifyVector = TRUE)
  ch <- as.data.frame(j$channels, stringsAsFactors = FALSE)
  ch
}

# Copy min/max/in_merge onto `target`, matching channels by label, then by index.
# Labels/colors always follow the current channel map.
apply_settings <- function(target, src) {
  for (i in seq_len(nrow(target))) {
    j <- match(tolower(target$label[i]), tolower(src$label))
    if (is.na(j)) j <- match(target$channel_index[i], src$channel_index)
    if (is.na(j)) next
    target$min[i] <- src$min[j]; target$max[i] <- src$max[j]
    if (!is.null(src$in_merge)) target$in_merge[i] <- as.logical(src$in_merge[j])
  }
  target
}

write_unit_settings <- function(cfg, u, st, source = "B&C app") {
  if (file.exists(u$file)) {                  # never lose a previous version
    hist <- file.path(settings_dir(cfg), "history"); dir.create(hist, showWarnings = FALSE)
    file.copy(u$file, file.path(hist, sub("\\.json$", paste0("_", timestamp_id(), ".json"), basename(u$file))))
  }
  st$color_hex <- color_hex(st$color)
  out <- list(
    session = cfg$session_name, experiment = u$experiment, group = u$group,
    saved = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"), source = source,
    display_mapping = "linear", projection = "max",
    note = "min/max are raw intensity values of the max projection (Fiji setMinAndMax). No gamma.",
    channels = st, images = I(u$uids))          # I(): always a list, even for one image
  jsonlite::write_json(out, u$file, auto_unbox = TRUE, pretty = TRUE, digits = NA)
}

write_settings_summary <- function(cfg, units, settings) {
  rows <- lapply(names(units), function(k) {
    u <- units[[k]]; st <- settings[[k]]
    data.frame(experiment = u$experiment, group = u$group, saved = file.exists(u$file),
               n_images = length(u$uids), channel_index = st$channel_index, label = st$label,
               color = st$color, min = st$min, max = st$max, in_merge = st$in_merge,
               stringsAsFactors = FALSE)
  })
  write_csv_safe(do.call(rbind, rows), file.path(settings_dir(cfg), "bc_settings_summary.csv"))
}

# ---- image composition (previews only; final rendering happens in Fiji) ----------
read_preview <- function(pp, uid) {
  f <- file.path(pp$raw, paste0(safe_uid(uid), ".tif"))
  if (!file.exists(f)) return(NULL)
  # ImageJ's own TIFF tags (LUTs, metadata) make libtiff warn; they are harmless here
  x <- suppressWarnings(tiff::readTIFF(f, all = TRUE, as.is = TRUE))
  if (!is.list(x)) x <- list(x)
  lapply(x, function(m) if (length(dim(m)) == 3) m[, , 1] else m)
}

scale_linear <- function(m, lo, hi) { v <- (m - lo) / max(hi - lo, 1e-9); v[v < 0] <- 0; v[v > 1] <- 1; v }

hex_rgb <- function(col) grDevices::col2rgb(color_hex(col))[, 1] / 255

compose_merge <- function(chs, st) {
  out <- array(0, c(nrow(chs[[1]]), ncol(chs[[1]]), 3))
  for (i in seq_len(nrow(st))) {
    ci <- st$channel_index[i]
    if (!isTRUE(st$in_merge[i]) || ci > length(chs)) next
    v <- scale_linear(chs[[ci]], st$min[i], st$max[i]); rgb <- hex_rgb(st$color[i])
    for (k in 1:3) out[, , k] <- out[, , k] + v * rgb[k]
  }
  out[out > 1] <- 1
  out
}

compose_channel <- function(m, lo, hi, color = NULL) {
  v <- scale_linear(m, lo, hi); rgb <- if (is.null(color)) c(1, 1, 1) else hex_rgb(color)
  array(c(v * rgb[1], v * rgb[2], v * rgb[3]), c(dim(v), 3))
}

# Side-by-side panels with a gap
strip_arrays <- function(arrs, gap = 6) {
  h <- max(vapply(arrs, function(a) dim(a)[1], 1)); w <- vapply(arrs, function(a) dim(a)[2], 1)
  out <- array(1, c(h, sum(w) + gap * (length(arrs) - 1), 3)); x <- 0
  for (i in seq_along(arrs)) { a <- arrs[[i]]; out[seq_len(dim(a)[1]), x + seq_len(w[i]), ] <- a; x <- x + w[i] + gap }
  out
}

downsample <- function(a, max_px) {
  f <- max(1, ceiling(max(dim(a)[1:2]) / max_px))
  a[seq(1, dim(a)[1], by = f), seq(1, dim(a)[2], by = f), , drop = FALSE]
}

png_file <- function(arr) { f <- tempfile(fileext = ".png"); png::writePNG(arr, f); f }

# ---- unattended mode: no app; saved settings are used, the rest per config bc.fallback --
#   auto     = 0 to the typical 99.9th percentile (labelled "auto" in the file)
#   template = copy from bc.template: a settings .json, or a settings/ folder from another
#              session (matched by group name without its "G01_" prefix, e.g. single_plane_10x_2048x2048)
#   skip     = leave the group without settings (render skips it)
tune_unattended <- function(cfg, units) {
  pol <- cfg$bc$fallback %||% "auto"; tpl <- cfg$bc$template %||% ""
  if (!pol %in% c("auto", "template", "skip")) stop("bc.fallback must be auto, template or skip", call. = FALSE)
  for (u in units) {
    if (u$saved) next                                   # already reported when loading
    if (pol == "skip") { msg("B&C ", u$key, ": no saved settings - will be skipped (bc.fallback = skip)"); next }
    st <- u$settings; src <- "auto"
    if (pol == "template") {
      tf <- find_bc_template(tpl, u$group)
      if (is.na(tf)) { msg("B&C ", u$key, ": no matching template in ", tpl, " - skipped"); next }
      st <- apply_settings(st, read_unit_settings(tf)); src <- paste0("template ", tf)
    }
    write_unit_settings(cfg, u, st, source = src)
    msg("B&C ", u$key, ": wrote ", src, " settings (max = ", paste(st$label, st$max, collapse = ", "), ")")
  }
  write_settings_summary(cfg, units, lapply(units, function(u)
    if (file.exists(u$file)) apply_settings(u$settings, read_unit_settings(u$file)) else u$settings))
  invisible(units)
}

find_bc_template <- function(tpl, group) {
  if (!nzchar(tpl)) return(NA_character_)
  tpl <- norm_path(tpl)
  if (file.exists(tpl) && !dir.exists(tpl)) return(tpl)
  if (!dir.exists(tpl)) { warning("bc.template not found: ", tpl, call. = FALSE); return(NA_character_) }
  files <- list.files(tpl, pattern = "\\.json$", full.names = TRUE)
  grp <- vapply(files, function(f) tryCatch(jsonlite::read_json(f)$group %||% NA_character_,
                                            error = function(e) NA_character_), "")
  strip <- function(x) sub("^G\\d+_", "", x)
  hit <- files[!is.na(grp) & strip(grp) == strip(group)]
  if (length(hit)) hit[1] else NA_character_
}
