# lif_reader.R — the pipeline's own .lif pixel locator (replaces Bio-Formats for pixels)
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# A .lif file is: [XML header] then a sequence of memory blocks, each labelled with an ID
# ("MemBlock_123"). Every image element in the XML names its block (<Memory MemoryBlockID=..>)
# and describes the pixel layout (<DimensionDescription BytesInc=..>, <ChannelDescription
# BytesInc=..>). Here we find each image's block BY ID and turn the layout into a compact
# string that Fiji (previews.groovy / render.groovy) uses to read pixels directly.
# Bio-Formats is still used for the inventory, but no longer for pixel data.
#
# Block header: int32 0x70 | int32 len | byte 0x2A | size (uint32 v1, uint64 v2+) |
#               byte 0x2A | int32 nChars | UTF-16LE id | data[size]

u32 <- function(x) ifelse(x < 0, x + 2^32, x)

lif_memory_blocks <- function(path, version = 2) {
  con <- file(path, "rb"); on.exit(close(con))
  total <- file.size(path)
  i32 <- function() readBin(con, "integer", size = 4, endian = "little")
  b1  <- function() readBin(con, "raw", 1)
  if (!identical(i32(), 112L)) stop("Not a Leica .lif file: ", path, call. = FALSE)
  i32(); b1(); n_xml <- u32(i32())
  pos <- 13 + 2 * n_xml                               # first memory block follows the XML
  ids <- character(); offs <- numeric(); sizes <- numeric()
  while (pos + 13 <= total) {
    seek(con, pos)
    if (!identical(i32(), 112L)) {
      warning(sprintf("Stopped reading .lif blocks at byte %.0f of %.0f (unexpected marker); %d block(s) found.",
                      pos, total, length(ids)), call. = FALSE)
      break
    }
    i32()
    if (!identical(b1(), as.raw(0x2A))) stop(sprintf("Bad .lif block header at byte %.0f", pos), call. = FALSE)
    if (version >= 2) { lo <- u32(i32()); hi <- u32(i32()); size <- lo + hi * 2^32; nsz <- 8 }
    else { size <- u32(i32()); nsz <- 4 }
    if (!identical(b1(), as.raw(0x2A))) stop(sprintf("Bad .lif block header at byte %.0f", pos), call. = FALSE)
    nid <- u32(i32())
    r  <- readBin(con, "raw", nid * 2)
    lo_bytes <- if (length(r)) r[seq(1, length(r), by = 2)] else raw()   # IDs are ASCII ("MemBlock_123")
    id <- rawToChar(lo_bytes[lo_bytes != as.raw(0)])
    data_off <- pos + 4 + 4 + 1 + nsz + 1 + 4 + 2 * nid
    ids <- c(ids, id); offs <- c(offs, data_off); sizes <- c(sizes, size)
    pos <- data_off + size
  }
  data.frame(id = ids, offset = offs, size = sizes, stringsAsFactors = FALSE)
}

# One row per XML image element (same rows/order as lif_image_table()$table)
lif_layout_table <- function(path) {
  doc <- read_lif_xml(path)
  ver <- suppressWarnings(as.integer(xml2::xml_attr(xml2::xml_root(doc), "Version")))
  if (is.na(ver)) ver <- 2L
  blocks <- lif_memory_blocks(path, ver)
  li <- lif_image_table(doc)
  rows <- lapply(seq_len(nrow(li$table)), function(k) {
    el <- li$nodes[[li$table$node_index[k]]]
    res <- tryCatch(element_layout(el, blocks), error = function(e) list(layout = NA_character_, error = conditionMessage(e)))
    data.frame(xml_index = k, path = li$table$path[k], layout = res$layout,
               sx = res$sx %||% NA, sy = res$sy %||% NA, sm = res$sm %||% NA,
               error = res$error %||% NA_character_, stringsAsFactors = FALSE)
  })
  do.call(rbind, rows)
}

element_layout <- function(el, blocks) {
  mem <- xml2::xml_find_first(el, "./Memory")
  bid <- xml2::xml_attr(mem, "MemoryBlockID")
  b <- match(bid, blocks$id)
  if (is.na(b)) stop("memory block '", bid, "' not found in file")
  img <- xml2::xml_find_first(el, "./Data/Image")
  dims <- xml2::xml_find_all(img, "./ImageDescription/Dimensions/DimensionDescription")
  did <- xml2::xml_attr(dims, "DimID"); n <- as.numeric(xml2::xml_attr(dims, "NumberOfElements"))
  inc <- as.numeric(xml2::xml_attr(dims, "BytesInc"))
  get <- function(id) { j <- which(did == id); if (length(j)) c(n[j[1]], inc[j[1]]) else c(1, 0) }
  other <- setdiff(did[n > 1], c("1", "2", "3", "4", "10"))
  if (length(other)) stop("unsupported dimension(s): ", paste(other, collapse = ","))
  X <- get("1"); Y <- get("2"); Z <- get("3"); Tt <- get("4"); M <- get("10")
  ch <- xml2::xml_find_all(img, "./ImageDescription/Channels/ChannelDescription")
  cinc <- as.numeric(xml2::xml_attr(ch, "BytesInc"))
  res  <- as.numeric(xml2::xml_attr(ch, "Resolution"))
  dtyp <- xml2::xml_attr(ch, "DataType")
  if (any(!is.na(dtyp) & dtyp != "0")) stop("non-integer pixel data (DataType ", paste(unique(dtyp), collapse = ","), ")")
  bps <- if (all(res <= 8, na.rm = TRUE)) 1 else 2
  if (max(res, na.rm = TRUE) > 16) stop("bit depth > 16 not supported")
  extent <- max(cinc) + (X[1] - 1) * X[2] + (Y[1] - 1) * Y[2] + (Z[1] - 1) * Z[2] +
            (Tt[1] - 1) * Tt[2] + (M[1] - 1) * M[2] + bps
  if (extent > blocks$size[b])
    stop(sprintf("layout needs %.0f bytes but memory block holds %.0f", extent, blocks$size[b]))
  f <- function(x) sprintf("%.0f", x)
  lay <- paste0("off=", f(blocks$offset[b]), ";bps=", bps, ";sx=", f(X[1]), ";sy=", f(Y[1]),
                ";sz=", f(Z[1]), ";sm=", f(M[1]), ";bx=", f(X[2]), ";by=", f(Y[2]), ";bz=", f(Z[2]),
                ";bm=", f(M[2]), ";cinc=", paste(f(cinc), collapse = ","))
  list(layout = lay, sx = X[1], sy = Y[1], sm = M[1], error = NA_character_)
}

parse_layout <- function(s) {
  kv <- strsplit(strsplit(s, ";", fixed = TRUE)[[1]], "=", fixed = TRUE)
  L <- stats::setNames(lapply(kv, `[`, 2), vapply(kv, `[`, "", 1))
  out <- lapply(L[setdiff(names(L), "cinc")], as.numeric)
  out$cinc <- as.numeric(strsplit(L$cinc, ",", fixed = TRUE)[[1]])
  out
}

pixel_reader_tag <- function(cfg) {
  if (cfg$input$mode == "bioformats") return("bioformats")
  if (cfg$input$mode != "lif") return("tif")
  if (identical(cfg$input$lif_reader %||% "direct", "bioformats")) "bioformats" else "lif-direct-1"
}

# Layout string per manifest row (NA = not available -> caller falls back to Bio-Formats)
lif_direct_layouts <- function(cfg, man) {
  out <- stats::setNames(rep(NA_character_, nrow(man)), man$image_uid)
  if (pixel_reader_tag(cfg) != "lif-direct-1") return(out)
  problems <- character()
  for (f in unique(man$source_file[man$source_mode %in% "lif"])) {
    tab <- tryCatch(lif_layout_table(f), error = function(e) {
      warning("Built-in .lif reader could not index ", basename(f), ": ", conditionMessage(e),
              " - falling back to Bio-Formats.", call. = FALSE); NULL })
    if (is.null(tab)) next
    for (i in which(man$source_file == f)) {
      k <- if (!is.null(man$xml_index) && !is.na(man$xml_index[i])) man$xml_index[i] else {
        hit <- which(tab$path == man$xml_path[i] %||% man$series_name[i]); if (length(hit) == 1) hit else NA }
      if (is.na(k) || k > nrow(tab)) { problems <- c(problems, paste0(man$image_uid[i], " (no XML match)")); next }
      t <- tab[k, ]
      if (is.na(t$layout)) { problems <- c(problems, paste0(man$image_uid[i], " (", t$error, ")")); next }
      if (!isTRUE(t$sx == man$size_x[i] && t$sy == man$size_y[i])) {
        problems <- c(problems, sprintf("%s (size %sx%s in XML vs %sx%s)", man$image_uid[i], t$sx, t$sy,
                                        man$size_x[i], man$size_y[i])); next }
      m <- if (identical(man$tile_role[i] %||% NA, "tile") && !is.na(man$tile_index[i])) man$tile_index[i] - 1 else 0
      if (m >= t$sm) { problems <- c(problems, paste0(man$image_uid[i], " (tile index out of range)")); next }
      out[i] <- paste0(t$layout, ";m=", m)
    }
  }
  if (length(problems))
    warning("Built-in .lif reader not used for: ", paste(problems, collapse = "; "),
            " - these use Bio-Formats instead.", call. = FALSE)
  out
}

# ---- R-side plane reader (used to validate against LAS X exports) -------------------
read_lif_plane <- function(path, L, c, z = 0, m = 0) {
  con <- file(path, "rb"); on.exit(close(con))
  base <- L$off + L$cinc[c] + z * L$bz + m * L$bm
  if (L$bx == L$bps && L$by == L$sx * L$bps) {
    seek(con, base)
    v <- readBin(con, "integer", n = L$sx * L$sy, size = L$bps, signed = FALSE, endian = "little")
    return(matrix(v, nrow = L$sy, byrow = TRUE))
  }
  out <- matrix(0L, L$sy, L$sx); span <- (L$sx - 1) * L$bx + L$bps; j <- (seq_len(L$sx) - 1) * L$bx
  for (y in seq_len(L$sy)) {
    seek(con, base + (y - 1) * L$by); b <- readBin(con, "raw", span)
    out[y, ] <- if (L$bps == 2) as.integer(b[j + 1]) + 256L * as.integer(b[j + 2]) else as.integer(b[j + 1])
  }
  out
}

# Compare the built-in reader with LAS X's own TIF exports, channel by channel.
#   validate_lif_reader("/path/config.yml", "/path/to/LASX/export/folder")
validate_lif_reader <- function(config_path, export_folder, include_only = TRUE) {
  require_pkgs(c("tiff", "xml2"))
  cfg <- read_config(config_path)
  if (cfg$input$mode != "lif") stop("validate_lif_reader needs a config whose input is a .lif", call. = FALSE)
  cfg$input$lif_reader <- "direct"
  man <- read_csv_safe(file.path(cfg$paths$manifest, "image_manifest.csv"))
  if (include_only) man <- man[as.logical(man$include) %in% TRUE, ]
  lay <- lif_direct_layouts(cfg, man)
  ex  <- list.files(norm_path(export_folder), pattern = "\\.tiff?$", recursive = TRUE, full.names = TRUE,
                    ignore.case = TRUE)
  px  <- parse_leica_tif_names(ex); px$full <- ex
  px  <- px[px$role == "channel", ]
  res <- list()
  for (i in seq_len(nrow(man))) {
    if (is.na(lay[i])) next
    L <- parse_layout(lay[i]); nm <- man$name_flat[i]
    for (c in seq_along(L$cinc)) for (k in which(px$base == nm & px$ch == c - 1)) {
      f <- px$full[k]; z <- if (is.na(px$z[k])) 0 else px$z[k]     # every exported z-slice is checked
      if (z >= L$sz) next
      e <- suppressWarnings(tiff::readTIFF(f[1], as.is = TRUE))
      if (length(dim(e)) == 3) e <- apply(e, c(1, 2), max)
      d <- read_lif_plane(cfg$input$path, L, c, z = z, m = L$m)
      same_size <- identical(dim(e), dim(d))
      r <- if (same_size && stats::sd(e) > 0 && stats::sd(d) > 0) stats::cor(as.vector(e), as.vector(d)) else NA
      res[[length(res) + 1]] <- data.frame(image_uid = man$image_uid[i], name = nm, channel = c, z = z,
        export_file = basename(f[1]), same_size = same_size,
        identical = same_size && all(e == d), max_abs_diff = if (same_size) max(abs(e - d)) else NA,
        correlation = round(r, 5), stringsAsFactors = FALSE)
    }
  }
  if (!length(res)) stop("No LAS X export files matched the images' names in ", export_folder, call. = FALSE)
  res <- do.call(rbind, res)
  res$verdict <- ifelse(res$identical, "identical", ifelse(!is.na(res$correlation) & res$correlation > 0.999,
                                                           "matches (export rescaled?)", "MISMATCH"))
  out <- file.path(cfg$paths$manifest, "lif_reader_validation.csv"); write_csv_safe(res, out)
  by_img <- tapply(res$verdict, res$name, function(v) if (all(v == "identical")) "identical"
                   else if (any(v == "MISMATCH")) "MISMATCH" else "matches")
  cat("\nBuilt-in .lif reader vs LAS X exports (", length(by_img), " images, ", nrow(res), " planes):\n", sep = "")
  for (n in names(by_img)) cat(sprintf("  %-28s %s\n", n, by_img[[n]]))
  cat("Details: ", out, "\n", sep = "")
  invisible(res)
}
