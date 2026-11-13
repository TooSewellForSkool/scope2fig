# review.R — Module 2: thumbnails + review app
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# Reads manifest/image_manifest.csv, makes previews (cached), opens the app.
# The app writes:
#   manifest/image_manifest.csv   your experiments, groups, include, fields
#   manifest/channel_map.csv      per experiment: channel label + display color
#   manifest/history/             a dated copy before every save
#   logs/review_*.json            what was saved and any open warnings

run_review <- function(cfg, pipeline_dir, launch = TRUE) {
  require_pkgs(c("shiny", "DT"))
  mpath <- file.path(cfg$paths$manifest, "image_manifest.csv")
  if (!file.exists(mpath)) stop("No manifest yet - run the 'inventory' step first.", call. = FALSE)
  man  <- prepare_manifest_for_review(read_csv_safe(mpath), cfg)
  chan <- read_csv_safe(file.path(cfg$paths$manifest, "channels.csv"))
  cmap <- build_channel_map(man, chan, file.path(cfg$paths$manifest, "channel_map.csv"))
  write_csv_safe(cmap, file.path(cfg$paths$manifest, "channel_map.csv"))

  pp <- run_previews(cfg, pipeline_dir, man, cmap)
  if (!launch) {
    if (all(man$needs_review %in% TRUE))
      msg("Review app skipped (unattended) and never saved: using automatic experiments, groups and ",
          "includes; your fields are blank.")
    else msg("Review app skipped (unattended): using your saved review.")
    return(invisible(list(manifest = man, channel_map = cmap)))
  }
  app <- review_app(cfg, man, chan, cmap, pp)
  msg("Opening the review app in your browser. Use 'Save & close' when done.")
  res <- shiny::runApp(app, launch.browser = TRUE)
  invisible(res)
}

# Columns the app edits must exist and be text; older manifests get new columns
prepare_manifest_for_review <- function(man, cfg) {
  uf <- unlist(cfg$user_fields)
  for (f in c(uf, "experiment", "group", "user_fields_source"))
    if (is.null(man[[f]])) man[[f]] <- NA_character_
  for (f in c(uf, "experiment", "group", "notes")) if (!is.null(man[[f]])) man[[f]] <- as.character(man[[f]])
  if (all(is.na(man$experiment))) {
    dyes <- if (is.null(man$dyes)) rep(NA_character_, nrow(man)) else man$dyes
    panel <- ifelse(is.na(dyes), paste0(man$size_c, " channels"), dyes)
    man$experiment <- man$experiment_auto <- paste0("E", match(panel, unique(panel)))
  }
  man$group[is.na(man$group)] <- man$group_auto[is.na(man$group)]
  if (is.null(man$session)) man$session <- cfg$session_name
  man$include <- as.logical(man$include)
  man
}

LUT_TO_COLOR <- c(blue = "blue", green = "green", red = "red", yellow = "yellow", cyan = "cyan",
                  magenta = "magenta", gray = "gray", grey = "gray", white = "gray")

# One row per experiment x channel. Keeps labels/colors you already set.
build_channel_map <- function(man, chan, existing_path = NULL) {
  rows <- lapply(unique(man$experiment), function(e) {
    u <- man$image_uid[man$experiment == e][1]
    ch <- chan[chan$image_uid == u, ]
    ch <- ch[order(ch$channel_index), ]
    n <- max(nrow(ch), suppressWarnings(max(man$size_c[man$experiment == e], na.rm = TRUE)), 0)
    if (n == 0) return(NULL)
    dye <- if (nrow(ch) && !is.null(ch$dye_name)) ch$dye_name[seq_len(n)] else rep(NA_character_, n)
    lut <- if (nrow(ch) && !is.null(ch$lut_name)) tolower(ch$lut_name[seq_len(n)]) else rep(NA_character_, n)
    col <- ifelse(grepl("^#[0-9a-f]{6}$", lut), toupper(lut), unname(LUT_TO_COLOR[lut]))   # Zeiss/OME give hex
    col[is.na(col)] <- rep_len(DEFAULT_COLORS, n)[is.na(col)]
    data.frame(experiment = e, channel_index = seq_len(n), dye_name = dye,
               label = ifelse(is.na(dye), paste0("C", seq_len(n)), dye),
               color = col, stringsAsFactors = FALSE)
  })
  cmap <- do.call(rbind, rows)
  if (!is.null(existing_path) && file.exists(existing_path)) {
    old <- read_csv_safe(existing_path)
    k_new <- paste(cmap$experiment, cmap$channel_index); k_old <- paste(old$experiment, old$channel_index)
    hit <- match(k_new, k_old)
    cmap$label[!is.na(hit)] <- old$label[hit[!is.na(hit)]]
    cmap$color[!is.na(hit)] <- old$color[hit[!is.na(hit)]]
  }
  cmap
}

# Problems worth fixing before B&C; shown in the app and written to the log
review_warnings <- function(man, cmap, cfg) {
  inc <- man[man$include %in% TRUE, ]
  w <- character()
  n_no_id <- sum(is.na(inc$experimental_id) | inc$experimental_id == "")
  if (n_no_id) w <- c(w, sprintf("%d included image(s) have no experimental_id.", n_no_id))
  units <- split(inc, paste(inc$experiment, inc$group, sep = " / "))
  for (u in names(units)) {
    k <- unique(units[[u]]$comparability_key)
    if (length(k) > 1) w <- c(w, sprintf("Group '%s' mixes images acquired differently (%d settings).", u, length(k)))
    lp <- unique(stats::na.omit(units[[u]]$laser_profile))
    if (length(lp) > 1) w <- c(w, sprintf("Group '%s' mixes laser profiles (%s).", u, paste(lp, collapse = ", ")))
  }
  miss <- setdiff(unique(inc$experiment), cmap$experiment)
  if (length(miss)) w <- c(w, sprintf("No channel names for experiment(s): %s.", paste(miss, collapse = ", ")))
  w
}

save_review <- function(cfg, man, cmap) {
  mpath <- file.path(cfg$paths$manifest, "image_manifest.csv")
  if (file.exists(mpath))
    file.copy(mpath, file.path(cfg$paths$history, sprintf("image_manifest_%s.csv", timestamp_id())))
  man$needs_review <- FALSE
  man <- order_manifest_columns(man, cfg)
  write_csv_safe(man, mpath)
  write_csv_safe(cmap, file.path(cfg$paths$manifest, "channel_map.csv"))
  warns <- review_warnings(man, cmap, cfg)
  write_run_log(cfg, "review", list(
    n_images = nrow(man), n_included = sum(man$include %in% TRUE),
    experiments = as.list(table(man$experiment[man$include %in% TRUE])),
    groups = as.list(table(paste(man$experiment, man$group, sep = " / ")[man$include %in% TRUE])),
    open_warnings = warns))
  invisible(warns)
}
