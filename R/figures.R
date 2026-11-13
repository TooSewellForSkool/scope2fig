# figures.R — Module 5: montages, comparison grids, PowerPoint
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# Reads renders/render_index.csv + each group's settings_used.json and writes to
# <output_root>/figures/:
#   montages/<experiment>/<group>/<name>_montage.png   channels (2-col grid) + larger merge,
#                                                      labels in channel colors, scale bar
#   grids/<experiment>__<group>_grid[_pN].png          rows = images (sorted by your fields),
#                                                      columns = channels + merge
#   <session>_figures.pptx                             grid slides, then one slide per image
#   figures_index.csv
# Only rendered PNGs are used (never the raw data), so nothing here changes intensities.

figure_defaults <- function() list(
  panel_px = 600, gap_px = 6, background = "white", label_size = 0,
  scale_bar_um = 0, scale_bar = TRUE, scale_bar_label = TRUE,
  inset_channel = "", caption = FALSE, pptx = TRUE, pptx_title_size = 30,
  grid_cell_px = 320, grid_max_rows = 6,
  grid_label_fields = c("experimental_id", "animal_replicate", "section_coverslip", "fov"),
  sort_fields = c("experimental_id", "animal_replicate", "section_coverslip", "fov")
)

run_figures <- function(cfg, pipeline_dir) {
  require_pkgs(c("magick", "jsonlite"))
  fo <- utils::modifyList(figure_defaults(), cfg$figures %||% list())
  idx_path <- file.path(render_dir(cfg), "render_index.csv")
  if (!file.exists(idx_path)) stop("No renders yet - run the 'render' step first.", call. = FALSE)
  idx <- read_csv_safe(idx_path)
  idx <- idx[is.na(idx$error) & !is.na(idx$files), ]
  if (!nrow(idx)) stop("No successfully rendered images found.", call. = FALSE)
  idx <- idx[do.call(order, c(lapply(c("experiment", "group", intersect(fo$sort_fields, names(idx)), "image_uid"),
                                     function(f) idx[[f]]), na.last = TRUE)), ]
  fig <- file.path(cfg$paths$root, "figures")
  dir.create(fig, recursive = TRUE, showWarnings = FALSE)

  out <- list(); grids <- list()
  for (k in unique(unit_key(idx$experiment, idx$group))) {
    rows <- idx[unit_key(idx$experiment, idx$group) == k, ]
    st <- read_unit_settings(file.path(rows$out_dir[1], "settings_used.json"))
    st <- st[order(st$channel_index), ]
    bar_um <- if (fo$scale_bar_um > 0) fo$scale_bar_um else nice_bar_um(rows)
    mdir <- file.path(fig, "montages", safe_uid(rows$experiment[1]), safe_uid(rows$group[1]))
    dir.create(mdir, recursive = TRUE, showWarnings = FALSE)
    msg("Montages for ", k, " (", nrow(rows), " image(s), scale bar ", bar_um, " µm)")
    for (i in seq_len(nrow(rows))) {
      r <- rows[i, ]
      m <- tryCatch(build_montage(r, st, fo, bar_um, cfg), error = function(e) {
        warning("Montage failed for ", r$image_uid, ": ", conditionMessage(e), call. = FALSE); NULL })
      if (is.null(m)) next
      f <- file.path(mdir, paste0(r$out_base, "_montage.png"))
      magick::image_write(m, f, format = "png")
      out[[length(out) + 1]] <- data.frame(image_uid = r$image_uid, experiment = r$experiment, group = r$group,
                                           montage = f, stringsAsFactors = FALSE)
    }
    pages <- split(seq_len(nrow(rows)), ceiling(seq_len(nrow(rows)) / fo$grid_max_rows))
    gdir <- file.path(fig, "grids"); dir.create(gdir, showWarnings = FALSE)
    for (p in seq_along(pages)) {
      g <- tryCatch(build_grid(rows[pages[[p]], ], st, fo, bar_um, cfg), error = function(e) {
        warning("Grid failed for ", k, ": ", conditionMessage(e), call. = FALSE); NULL })
      if (is.null(g)) next
      f <- file.path(gdir, paste0(safe_uid(rows$experiment[1]), "__", safe_uid(rows$group[1]), "_grid",
                                  if (length(pages) > 1) paste0("_p", p) else "", ".png"))
      magick::image_write(g, f, format = "png")
      grids[[length(grids) + 1]] <- data.frame(key = k, page = p, file = f, stringsAsFactors = FALSE)
    }
  }
  out <- do.call(rbind, out); grids <- do.call(rbind, grids)
  if (!is.null(out)) write_csv_safe(merge(out, idx[, setdiff(names(idx), c("experiment", "group"))],
                                          by = "image_uid", sort = FALSE), file.path(fig, "figures_index.csv"))
  pptx <- NULL
  if (isTRUE(fo$pptx)) {
    if (requireNamespace("officer", quietly = TRUE)) pptx <- build_pptx(cfg, fig, out, grids, idx, fo)
    else msg("Skipping PowerPoint: install it with install.packages(\"officer\").")
  }
  write_run_log(cfg, "figures", list(montages = nrow(out %||% data.frame()), grids = nrow(grids %||% data.frame()),
                                     pptx = pptx, options = fo))
  msg("Figures are in ", fig)
  invisible(list(montages = out, grids = grids, pptx = pptx))
}

# ---- helpers -------------------------------------------------------------------
nice_bar_um <- function(rows) {
  # ~1/5 of the image width, rounded down to a "nice" number; same for the whole group
  px <- stats::median(rows$pixel_size_um, na.rm = TRUE)
  w  <- suppressWarnings(as.numeric(magick::image_info(magick::image_read(merge_file(rows[1, ])))$width))
  if (!is.finite(px) || !is.finite(w)) return(NA)
  target <- w * px / 5
  nice <- c(1, 2, 5, 10, 20, 25, 50, 100, 200, 250, 500, 1000, 2000, 5000)
  max(nice[nice <= target], 1)
}

render_files <- function(r) strsplit(r$files, ";", fixed = TRUE)[[1]]
merge_file   <- function(r) file.path(r$out_dir, paste0(r$out_base, "_merge.png"))
channel_file <- function(r, i) {
  f <- render_files(r)
  f <- f[startsWith(f, paste0(r$out_base, "_C", i, "-")) & endsWith(f, ".png")]
  if (length(f)) file.path(r$out_dir, f[1]) else NA_character_
}
label_color <- function(col) { h <- color_hex(col); if (h %in% c("#FFFFFF", "#808080")) "white" else h }

hcat <- function(imgs, gap, bg) {
  h <- max(magick::image_info(do.call(c, imgs))$height)
  parts <- list()
  for (i in seq_along(imgs)) {
    w <- magick::image_info(imgs[[i]])$width
    parts[[length(parts) + 1]] <- magick::image_extent(imgs[[i]], paste0(w, "x", h), gravity = "north", color = bg)
    if (i < length(imgs) && gap > 0) parts[[length(parts) + 1]] <- magick::image_blank(gap, h, bg)
  }
  magick::image_append(do.call(c, parts))
}
vcat <- function(imgs, gap, bg) {
  w <- max(magick::image_info(do.call(c, imgs))$width)
  parts <- list()
  for (i in seq_along(imgs)) {
    h <- magick::image_info(imgs[[i]])$height
    parts[[length(parts) + 1]] <- magick::image_extent(imgs[[i]], paste0(w, "x", h), gravity = "west", color = bg)
    if (i < length(imgs) && gap > 0) parts[[length(parts) + 1]] <- magick::image_blank(w, gap, bg)
  }
  magick::image_append(do.call(c, parts), stack = TRUE)
}

add_label <- function(img, text, col, size) {
  pad <- max(4, round(size * 0.4))
  magick::image_annotate(img, text, color = col, size = size, weight = 700, gravity = "northwest",
                         location = paste0("+", pad, "+", pad))
}

add_scale_bar <- function(img, orig_width, px_um, bar_um, show_label) {
  w <- magick::image_info(img)$width
  if (!is.finite(px_um) || !is.finite(bar_um) || px_um <= 0) return(img)
  bar_px <- round(bar_um / px_um * w / orig_width)
  if (bar_px < 2 || bar_px > w * 0.9) return(img)
  thick <- max(3, round(w * 0.012)); margin <- max(6, round(w * 0.03))
  img <- magick::image_composite(img, magick::image_blank(bar_px, thick, "white"), gravity = "southeast",
                                 offset = paste0("+", margin, "+", margin))
  if (show_label)
    img <- magick::image_annotate(img, paste(bar_um, "µm"), color = "white", size = max(10, round(w * 0.035)),
                                  weight = 700, gravity = "southeast",
                                  location = paste0("+", margin, "+", margin + thick + 2))
  img
}

field_text <- function(r, fields) {
  v <- vapply(intersect(fields, names(r)), function(f) as.character(r[[f]]), "")
  v <- v[!is.na(v) & nzchar(v)]
  v
}

build_montage <- function(r, st, fo, bar_um, cfg) {
  mf <- merge_file(r); if (!file.exists(mf)) stop("missing ", basename(mf))
  merge <- magick::image_read(mf); info <- magick::image_info(merge)
  W0 <- info$width; H0 <- info$height
  pw <- fo$panel_px; ph <- round(pw * H0 / W0); gap <- fo$gap_px; bg <- fo$background
  ls <- if (fo$label_size > 0) fo$label_size else max(14, round(pw * 0.06))

  panels <- lapply(seq_len(nrow(st)), function(i) {
    f <- channel_file(r, st$channel_index[i])
    im <- if (!is.na(f) && file.exists(f)) magick::image_scale(magick::image_read(f), paste0(pw, "x", ph, "!"))
          else magick::image_blank(pw, ph, "gray20")
    add_label(im, st$label[i], label_color(st$color[i]), ls)
  })
  n <- length(panels); ncol <- if (n <= 1) 1 else 2
  if (n %% ncol) panels <- c(panels, list(magick::image_blank(pw, ph, bg)))
  rows <- split(panels, ceiling(seq_along(panels) / ncol))
  block <- vcat(lapply(rows, hcat, gap = gap, bg = bg), gap, bg)
  bh <- magick::image_info(block)$height

  mw <- round(bh * W0 / H0)
  m <- magick::image_scale(merge, paste0(mw, "x", bh, "!"))
  m <- add_label(m, "Merge", "white", ls)
  if (nzchar(fo$inset_channel)) {
    j <- match(tolower(fo$inset_channel), tolower(st$label))
    f <- if (!is.na(j)) channel_file(r, st$channel_index[j]) else NA
    if (!is.na(f) && file.exists(f)) {
      ins <- magick::image_scale(magick::image_read(f), paste0(round(mw / 3)))
      ins <- magick::image_border(add_label(ins, st$label[j], label_color(st$color[j]), round(ls * 0.7)), "white", "2x2")
      margin <- max(6, round(mw * 0.03))
      m <- magick::image_composite(m, ins, gravity = "southwest", offset = paste0("+", margin, "+", margin))
    }
  }
  if (isTRUE(fo$scale_bar)) m <- add_scale_bar(m, W0, r$pixel_size_um, bar_um, isTRUE(fo$scale_bar_label))
  out <- hcat(list(block, m), gap, bg)
  if (isTRUE(fo$caption)) {
    txt <- paste(c(r$out_base, field_text(r, fo$grid_label_fields), r$group), collapse = "  |  ")
    cap <- magick::image_annotate(magick::image_blank(magick::image_info(out)$width, round(ls * 1.8), bg),
                                  txt, color = "black", size = round(ls * 0.8), gravity = "west", location = "+8+0")
    out <- vcat(list(out, cap), 0, bg)
  }
  out
}

build_grid <- function(rows, st, fo, bar_um, cfg) {
  cw <- fo$grid_cell_px; gap <- fo$gap_px; bg <- fo$background
  ls <- max(12, round(cw * 0.07)); lw <- round(cw * 0.9)
  cols <- c(as.list(seq_len(nrow(st))), list("merge"))
  header_cell <- function(text, col) {
    im <- magick::image_blank(cw, round(ls * 2.2), bg)
    im <- magick::image_annotate(im, text, color = "#222222", size = ls, weight = 700, gravity = "center")
    magick::image_composite(im, magick::image_blank(round(cw * 0.6), max(3, round(ls * 0.2)),
                                                    if (label_color(col) == "white") "#888888" else color_hex(col)),
                            gravity = "south", offset = "+0+2")
  }
  header <- hcat(c(list(magick::image_blank(lw, round(ls * 2.2), bg)),
                   lapply(seq_len(nrow(st)), function(i) header_cell(st$label[i], st$color[i])),
                   list(header_cell("Merge", "white"))), gap, bg)
  body <- lapply(seq_len(nrow(rows)), function(k) {
    r <- rows[k, ]; info <- magick::image_info(magick::image_read(merge_file(r)))
    chh <- round(cw * info$height / info$width)
    cells <- lapply(cols, function(cc) {
      f <- if (identical(cc, "merge")) merge_file(r) else channel_file(r, st$channel_index[cc])
      im <- if (!is.na(f) && file.exists(f)) magick::image_scale(magick::image_read(f), paste0(cw, "x", chh, "!"))
            else magick::image_blank(cw, chh, "gray20")
      if (identical(cc, "merge") && k == 1 && isTRUE(fo$scale_bar))
        im <- add_scale_bar(im, info$width, r$pixel_size_um, bar_um, isTRUE(fo$scale_bar_label))
      im
    })
    lab <- field_text(r, fo$grid_label_fields); if (!length(lab)) lab <- r$out_base
    lab_img <- magick::image_annotate(magick::image_blank(lw, chh, bg), paste(lab, collapse = "\n"),
                                      color = "#222222", size = ls, gravity = "west", location = "+6+0")
    hcat(c(list(lab_img), cells), gap, bg)
  })
  ttl <- paste(rows$experiment[1], "/", rows$group[1])
  title <- magick::image_annotate(magick::image_blank(lw + length(cols) * (cw + gap), round(ls * 2), bg), ttl,
                                  color = "#222222", size = round(ls * 1.1), weight = 700, gravity = "west",
                                  location = "+6+0")
  magick::image_border(vcat(c(list(title, header), body), gap, bg), bg, "10x10")
}

build_pptx <- function(cfg, fig, montages, grids, idx, fo) {
  doc <- officer::read_pptx()
  ss <- officer::slide_size(doc)
  fit <- function(path, box_w, box_h) {
    i <- magick::image_info(magick::image_read(path)); s <- min(box_w / i$width, box_h / i$height)
    c(i$width * s, i$height * s)
  }
  add_img_slide <- function(title, path, foot = NULL) {
    doc <<- officer::add_slide(doc, layout = "Title Only", master = "Office Theme")
    doc <<- officer::ph_with(doc, officer::fpar(officer::ftext(title, officer::fp_text(font.size = fo$pptx_title_size))),
                             location = officer::ph_location_type(type = "title"))
    top <- 1.35; bw <- ss$width - 0.6; bh <- ss$height - top - (if (is.null(foot)) 0.25 else 0.6)
    wh <- fit(path, bw, bh)
    doc <<- officer::ph_with(doc, officer::external_img(path, width = wh[1], height = wh[2]),
                             location = officer::ph_location(left = (ss$width - wh[1]) / 2, top = top,
                                                             width = wh[1], height = wh[2]))
    if (!is.null(foot))
      doc <<- officer::ph_with(doc, officer::fpar(officer::ftext(foot, officer::fp_text(font.size = 10, color = "#555555"))),
                               location = officer::ph_location(left = 0.3, top = ss$height - 0.5,
                                                               width = ss$width - 0.6, height = 0.4))
  }
  for (k in unique(grids$key %||% character())) {
    g <- grids[grids$key == k, ]
    for (p in seq_len(nrow(g))) add_img_slide(paste0(k, if (nrow(g) > 1) paste0(" (", p, "/", nrow(g), ")") else ""), g$file[p])
  }
  for (i in seq_len(nrow(montages %||% data.frame()))) {
    r <- idx[idx$image_uid == montages$image_uid[i], ][1, ]
    st <- read_unit_settings(file.path(r$out_dir, "settings_used.json"))
    title <- paste(c(r$out_base, field_text(r, fo$grid_label_fields)), collapse = "  ·  ")
    foot <- paste0(r$experiment, " / ", r$group, "   |   B&C (linear): ",
                   paste0(st$label, " ", st$min, "–", st$max, collapse = ", "))
    add_img_slide(title, montages$montage[i], foot)
  }
  path <- file.path(fig, paste0(safe_uid(cfg$session_name), "_figures.pptx"))
  print(doc, target = path)
  msg("PowerPoint: ", path)
  path
}
