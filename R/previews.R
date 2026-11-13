# previews.R — thumbnails (for recognising images) + small raw previews (for the
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# B&C step). Generated once by Fiji and cached in <output_root>/previews/.

safe_uid <- function(uid) gsub("[^A-Za-z0-9_.-]+", "_", uid)

preview_paths <- function(cfg) {
  d <- file.path(cfg$paths$root, "previews")
  list(dir = d, raw = file.path(d, "raw"), thumbs = file.path(d, "thumbs"),
       stats = file.path(d, "preview_stats.csv"), index = file.path(d, "preview_index.csv"))
}

COLOR_NAMES <- c(gray = "#FFFFFF", grey = "#FFFFFF", white = "#FFFFFF", red = "#FF0000",
                 green = "#00FF00", blue = "#0000FF", cyan = "#00FFFF", magenta = "#FF00FF",
                 yellow = "#FFFF00")
DEFAULT_COLORS <- c("gray", "cyan", "magenta", "yellow", "red", "green", "blue")

color_hex <- function(x) {
  x <- tolower(trimws(as.character(x)))
  out <- ifelse(grepl("^#[0-9a-f]{6}$", x), toupper(x), unname(COLOR_NAMES[x]))
  ifelse(is.na(out), "#FFFFFF", out)
}

# Colors for one image, in channel order, from the experiment's channel map
image_colors <- function(experiment, n_ch, cmap) {
  m <- cmap[cmap$experiment == experiment, ]
  m <- m[order(m$channel_index), ]
  cols <- if (nrow(m)) m$color[seq_len(min(n_ch, nrow(m)))] else character()
  if (length(cols) < n_ch) cols <- c(cols, rep_len(DEFAULT_COLORS, n_ch)[(length(cols) + 1):n_ch])
  paste(color_hex(cols), collapse = ";")
}

# Large images (e.g. stitched tile scans) get bigger previews so detail survives
desired_preview_size <- function(cfg, man) {
  p <- cfg$previews %||% list()
  small <- p$max_size %||% 512; large <- p$large_max_size %||% 1536
  ifelse(pmax(man$size_x, man$size_y, na.rm = TRUE) > (p$large_threshold_px %||% 3000), large, small)
}

build_preview_jobs <- function(cfg, man, cmap, uids, layouts = NULL) {
  rows <- list()
  sizes <- stats::setNames(desired_preview_size(cfg, man), man$image_uid)
  for (u in uids) {
    i <- match(u, man$image_uid)
    cols <- image_colors(man$experiment[i], man$size_c[i] %||% 4, cmap)
    s <- safe_uid(u)
    if (man$source_mode[i] %in% c("lif", "bioformats")) {
      lay <- if (!is.null(layouts)) layouts[[u]] else NA_character_
      rows[[length(rows) + 1]] <- data.frame(uid_safe = s, kind = if (is.na(lay)) "bf" else "lifraw",
        path = man$source_file[i], series = man$series_index[i], channel = "", z = "", colors = cols,
        lif = if (is.na(lay)) "" else lay)
    } else if (identical(man$layout[i], "single_file")) {
      rows[[length(rows) + 1]] <- data.frame(uid_safe = s, kind = "bf",
        path = file.path(cfg$input$path, man$files[i]), series = 0, channel = "", z = "", colors = cols, lif = "")
    } else if (man$layout[i] %in% c("split_channels", "split_channels_z")) {
      idx <- parse_leica_tif_names(strsplit(man$files[i], ";", fixed = TRUE)[[1]])
      idx <- idx[idx$role == "channel", ]
      rows[[length(rows) + 1]] <- data.frame(uid_safe = s, kind = "split",
        path = file.path(cfg$input$path, idx$file), series = "", channel = idx$ch,
        z = ifelse(is.na(idx$z), 0L, idx$z), colors = cols, lif = "")
    }
  }
  if (!length(rows)) return(NULL)
  jobs <- do.call(rbind, rows)
  jobs$max_size <- unname(sizes[match(jobs$uid_safe, safe_uid(names(sizes)))])
  jobs
}

# Make previews for images that don't have one yet (or all, with force = TRUE)
run_previews <- function(cfg, pipeline_dir, man, cmap, force = FALSE, max_size = 512) {
  pp <- preview_paths(cfg)
  dir.create(pp$dir, recursive = TRUE, showWarnings = FALSE)
  todo <- man$image_uid[man$layout != "overlay_only"]
  # previews made with a different pixel reader (e.g. Bio-Formats before the built-in .lif reader) are redone
  tag <- pixel_reader_tag(cfg); tag_file <- file.path(pp$dir, "pixel_reader.txt")
  old_tag <- if (file.exists(tag_file)) readLines(tag_file, warn = FALSE)[1]
             else if (file.exists(pp$index) && cfg$input$mode == "lif") "bioformats" else tag
  if (!identical(old_tag, tag)) {
    msg("Pixel reader changed (", old_tag, " -> ", tag, "): regenerating all previews.")
    force <- TRUE
  }
  if (!force) {
    missing <- !file.exists(file.path(pp$thumbs, paste0(safe_uid(todo), ".png")))
    # also redo previews made smaller than the current size rule (e.g. before large-image previews)
    too_small <- rep(FALSE, length(todo))
    if (file.exists(pp$index)) {
      idx <- read_csv_safe(pp$index); want <- desired_preview_size(cfg, man[match(todo, man$image_uid), ])
      j <- match(safe_uid(todo), idx$uid_safe)
      have <- pmax(idx$preview_width[j], idx$preview_height[j])
      full <- pmax(idx$full_width[j], idx$full_height[j])
      too_small <- !is.na(have) & have < pmin(want, full) - 1
    }
    todo <- todo[missing | too_small]
  }
  if (length(todo)) {
    lay <- lif_direct_layouts(cfg, man[man$image_uid %in% todo, ])
    jobs <- build_preview_jobs(cfg, man, cmap, todo, lay)
    jobs_file <- file.path(pp$dir, "preview_jobs.tsv")
    utils::write.table(jobs, jobs_file, sep = "\t", quote = FALSE, row.names = FALSE, fileEncoding = "UTF-8")
    unlink(file.path(pp$dir, "previews_done.txt"))
    msg("Making previews for ", length(todo), " image(s) (cached for later runs)...")
    run_fiji_script(cfg, file.path(pipeline_dir, "fiji", "previews.groovy"),
      params = list(jobs_tsv = jobs_file, out_dir = pp$dir, max_size = max_size),
      log_file = file.path(cfg$paths$logs, sprintf("fiji_previews_%s.log", timestamp_id())),
      expect_file = file.path(pp$dir, "previews_done.txt"))
    merge_run_table(file.path(pp$dir, "preview_index_run.tsv"), pp$index)
    merge_run_table(file.path(pp$dir, "preview_stats_run.tsv"), pp$stats)
    writeLines(tag, tag_file)
  } else msg("Previews already exist for all images (cached).")
  if (file.exists(pp$stats)) check_duplicate_pixels(pp$stats, safe_uid(man$image_uid))
  idx <- if (file.exists(pp$index)) read_csv_safe(pp$index) else NULL
  if (!is.null(idx) && any(!is.na(idx$error)))
    warning("Preview failed for: ", paste(idx$uid_safe[!is.na(idx$error)], collapse = ", "),
            " (see logs/fiji_previews_*.log)", call. = FALSE)
  invisible(pp)
}

# Replace rows for re-generated images, keep the rest
merge_run_table <- function(run_tsv, out_csv) {
  if (!file.exists(run_tsv)) return(invisible())
  new <- utils::read.delim(run_tsv, stringsAsFactors = FALSE, na.strings = "", quote = "")
  if (file.exists(out_csv)) {
    old <- read_csv_safe(out_csv)
    old <- old[!old$uid_safe %in% new$uid_safe, , drop = FALSE]
    new <- rbind(old[, names(new)], new)
  }
  write_csv_safe(new, out_csv)
}

# Two different images should never have identical statistics in every channel. If they
# do, one was probably read from the other's data (the Bio-Formats .lif problem).
check_duplicate_pixels <- function(stats_csv, uids_safe) {
  st <- read_csv_safe(stats_csv)
  st <- st[st$uid_safe %in% uids_safe, ]
  if (!nrow(st)) return(invisible(list()))
  st <- st[order(st$uid_safe, st$channel_index), ]
  sig <- tapply(seq_len(nrow(st)), st$uid_safe, function(i)
    paste(format(st$mean[i], digits = 12), st$max[i], format(st$p999[i], digits = 8), collapse = "|"))
  d <- Filter(function(x) length(x) > 1, split(names(sig), unname(sig)))
  if (length(d))
    warning("Identical pixel statistics in every channel for: ",
            paste(vapply(d, paste, "", collapse = " = "), collapse = "; "),
            ". These images were probably read from the same data - do not use them until resolved.",
            call. = FALSE)
  invisible(d)
}
