# render.R — Module 4: apply saved B&C settings at full resolution (via Fiji)
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# Reads settings/<experiment>__<group>.json (from the tune step) and writes, per image,
# into renders/<experiment>/<group>/:
#   <name>_composite.tif      unaltered pixel data (quantify from this or the raw .lif),
#                             with channel colors + min/max stored as display settings
#   <name>_C<n>-<label>.png   each channel, linear min/max, 8-bit
#   <name>_merge.png          merge of the "in merge" channels
#   <name>_stack.tif          (optional) full z-stack, unaltered
#   settings_used.json        copy of the settings that produced the files
# z-stacks get "_MAX" in <name>. renders/render_index.csv lists every output with
# your fields - the figures step reads it. Images whose settings haven't changed since
# the last render are skipped (use force = TRUE to redo everything).

render_dir <- function(cfg) file.path(cfg$paths$root, "renders")
# Bump when render output changes, so existing renders are redone automatically
RENDER_VERSION <- "3"   # 2: grayscale PNGs drop the LAS X LUT; 3: pixel reader recorded

run_render <- function(cfg, pipeline_dir, force = FALSE) {
  require_pkgs("jsonlite")
  man <- prepare_manifest_for_review(read_csv_safe(file.path(cfg$paths$manifest, "image_manifest.csv")), cfg)
  inc <- man[man$include %in% TRUE, ]
  rd <- render_dir(cfg); dir.create(rd, recursive = TRUE, showWarnings = FALSE)
  ropt <- cfg$render %||% list()
  png_mode <- ropt$channel_png %||% "gray"
  save_stack <- isTRUE(ropt$save_stack)

  # settings per experiment/group
  keys <- unique(unit_key(inc$experiment, inc$group))
  todo <- list(); skipped_units <- character()
  idx_path <- file.path(rd, "render_index.csv")
  old_idx <- if (file.exists(idx_path)) read_csv_safe(idx_path) else NULL
  for (k in keys) {
    rows <- inc[unit_key(inc$experiment, inc$group) == k, ]
    f <- unit_file(cfg, rows$experiment[1], rows$group[1])
    if (!file.exists(f)) { skipped_units <- c(skipped_units, k); next }
    st <- read_unit_settings(f)
    st <- st[order(st$channel_index), ]
    sig <- paste(paste(st$label, st$color, st$min, st$max, st$in_merge, collapse = "|"), png_mode, save_stack,
                 "v", RENDER_VERSION, pixel_reader_tag(cfg))
    out_dir <- file.path(rd, safe_uid(rows$experiment[1]), safe_uid(rows$group[1]))
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    file.copy(f, file.path(out_dir, "settings_used.json"), overwrite = TRUE)
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      name <- safe_uid(r$name_flat %||% r$series_name)
      if (isTRUE(r$size_z > 1)) name <- paste0(name, "_MAX")
      base <- file.path(out_dir, name)
      prev <- if (!is.null(old_idx)) old_idx[old_idx$image_uid == r$image_uid, ] else NULL
      up_to_date <- !force && !is.null(prev) && nrow(prev) == 1 && identical(prev$settings_sig, sig) &&
                    file.exists(paste0(base, "_merge.png"))
      todo[[length(todo) + 1]] <- list(row = r, st = st, sig = sig, base = base, run = !up_to_date)
    }
  }
  if (length(skipped_units))
    warning("No saved B&C settings (skipped): ", paste(skipped_units, collapse = "; "),
            " - open them in the tune step and save.", call. = FALSE)
  if (!length(todo)) stop("Nothing to render.", call. = FALSE)

  run_items <- Filter(function(x) x$run, todo)
  if (length(run_items)) {
    # name clashes (two images flattening to the same name) get the image index appended
    bases <- vapply(run_items, `[[`, "", "base")
    dup <- duplicated(bases) | duplicated(bases, fromLast = TRUE)
    for (i in which(dup)) run_items[[i]]$base <- paste0(run_items[[i]]$base, "_", safe_uid(run_items[[i]]$row$image_uid))
    lay <- lif_direct_layouts(cfg, do.call(rbind, lapply(run_items, `[[`, "row")))
    jobs <- do.call(rbind, lapply(run_items, function(x) render_job_rows(cfg, x, png_mode, save_stack,
                                                                          lay[[x$row$image_uid]])))
    jobs_file <- file.path(rd, "render_jobs.tsv")
    utils::write.table(jobs, jobs_file, sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
    unlink(file.path(rd, "render_done.txt"))
    msg("Rendering ", length(run_items), " image(s) at full resolution (",
        length(todo) - length(run_items), " already up to date)...")
    run_fiji_script(cfg, file.path(pipeline_dir, "fiji", "render.groovy"),
      params = list(jobs_tsv = jobs_file, out_dir = rd),
      log_file = file.path(cfg$paths$logs, sprintf("fiji_render_%s.log", timestamp_id())),
      expect_file = file.path(rd, "render_done.txt"))
    todo <- lapply(todo, function(x) {           # keep any de-duplicated base names
      m <- Filter(function(y) identical(y$row$image_uid, x$row$image_uid), run_items)
      if (length(m)) m[[1]] else x
    })
  } else msg("All ", length(todo), " image(s) are already rendered with the current settings.")

  idx <- update_render_index(cfg, todo, old_idx, file.path(rd, "render_run.tsv"),
                             any_run = length(run_items) > 0)
  write_csv_safe(idx, idx_path)

  hot <- idx[!is.na(idx$max_saturation_pct) & idx$max_saturation_pct > 1, ]
  if (nrow(hot)) msg("Note: ", nrow(hot), " image(s) have >1% clipped pixels in a channel at full resolution ",
                     "(see saturation_pct in renders/render_index.csv).")
  failed <- idx[!is.na(idx$error), ]
  if (nrow(failed)) warning("Render failed for: ", paste(failed$image_uid, collapse = ", "),
                            " (see logs/fiji_render_*.log)", call. = FALSE)
  write_run_log(cfg, "render", list(rendered = length(run_items), up_to_date = length(todo) - length(run_items),
    skipped_groups = skipped_units, failed = failed$image_uid, output = rd))
  msg("Renders are in ", rd)
  invisible(idx)
}

render_job_rows <- function(cfg, x, png_mode, save_stack, layout = NA_character_) {
  r <- x$row; st <- x$st
  common <- data.frame(uid_safe = safe_uid(r$image_uid), out_base = x$base,
    labels = paste(st$label, collapse = ";"), mins = paste(st$min, collapse = ";"),
    maxs = paste(st$max, collapse = ";"), colors = paste(color_hex(st$color), collapse = ";"),
    in_merge = paste(ifelse(as.logical(st$in_merge), 1, 0), collapse = ";"),
    pixel_size_um = r$pixel_size_um %||% 0, channel_png = png_mode,
    save_stack = as.integer(save_stack), lif = if (is.na(layout)) "" else layout, stringsAsFactors = FALSE)
  if (r$source_mode %in% c("lif", "bioformats")) {
    cbind(common, kind = if (is.na(layout)) "bf" else "lifraw", path = r$source_file, series = r$series_index,
          channel = "", z = "")
  } else if (identical(r$layout, "single_file")) {
    cbind(common, kind = "bf", path = file.path(cfg$input$path, r$files), series = 0, channel = "", z = "")
  } else {
    p <- parse_leica_tif_names(strsplit(r$files, ";", fixed = TRUE)[[1]]); p <- p[p$role == "channel", ]
    cbind(common[rep(1, nrow(p)), ], kind = "split", path = file.path(cfg$input$path, p$file), series = "",
          channel = p$ch, z = ifelse(is.na(p$z), 0L, p$z), row.names = NULL)
  }
}

update_render_index <- function(cfg, todo, old_idx, run_tsv, any_run) {
  run <- if (any_run && file.exists(run_tsv))
    utils::read.delim(run_tsv, stringsAsFactors = FALSE, na.strings = "", quote = "", colClasses = "character")
    else NULL
  uf <- unlist(cfg$user_fields)
  rows <- lapply(todo, function(x) {
    r <- x$row
    if (x$run && !is.null(run)) {
      rr <- run[run$uid_safe == safe_uid(r$image_uid), ]
      if (!nrow(rr)) return(NULL)
      sat <- suppressWarnings(as.numeric(strsplit(rr$saturation_pct %||% "", ";")[[1]]))
      base <- data.frame(files = rr$files, saturation_pct = rr$saturation_pct %||% NA,
        max_saturation_pct = if (length(sat) && any(!is.na(sat))) max(sat, na.rm = TRUE) else NA,
        error = rr$error, rendered = format(Sys.time(), "%Y-%m-%d %H:%M:%S"), stringsAsFactors = FALSE)
    } else {
      p <- old_idx[old_idx$image_uid == r$image_uid, ][1, ]
      base <- p[, c("files", "saturation_pct", "max_saturation_pct", "error", "rendered")]
    }
    data.frame(image_uid = r$image_uid, session = r$session, experiment = r$experiment, group = r$group,
               r[, intersect(uf, names(r)), drop = FALSE], series_name = r$series_name,
               acq_type = r$acq_type, objective_mag = r$objective_mag, pixel_size_um = r$pixel_size_um,
               size_z = r$size_z, out_dir = dirname(x$base), out_base = basename(x$base), base,
               settings_sig = x$sig, stringsAsFactors = FALSE, row.names = NULL)
  })
  do.call(rbind, rows)
}
