# utils.R — config loading, paths, small helpers shared by all modules
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0 || (length(a) == 1 && (is.na(a) || identical(a, "")))) b else a
}

msg <- function(...) message(format(Sys.time(), "[%H:%M:%S] "), ...)

require_pkgs <- function(pkgs) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    stop("Missing R packages: ", paste(missing, collapse = ", "),
         "\nInstall with: install.packages(c(",
         paste0('"', missing, '"', collapse = ", "), "))", call. = FALSE)
  }
}

norm_path <- function(p) normalizePath(path.expand(p), winslash = "/", mustWork = FALSE)

timestamp_id <- function() format(Sys.time(), "%Y%m%d-%H%M%S")

write_csv_safe <- function(df, path) {
  dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(df, path, row.names = FALSE, na = "", fileEncoding = "UTF-8")
}

read_csv_safe <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE,
                  na.strings = "", fileEncoding = "UTF-8")
}

# Named-capture regex -> data.frame (one column per named group, NA if absent)
regex_named <- function(x, pattern) {
  m  <- regexpr(pattern, x, perl = TRUE)
  st <- attr(m, "capture.start"); ln <- attr(m, "capture.length")
  nm <- attr(m, "capture.names")
  keep <- nzchar(nm)
  out <- lapply(which(keep), function(j) {
    v <- substr(x, st[, j], st[, j] + ln[, j] - 1)
    v[m < 0 | st[, j] < 1 | ln[, j] < 1] <- NA_character_
    v
  })
  out <- as.data.frame(out, stringsAsFactors = FALSE)
  names(out) <- nm[keep]
  out
}

# ---- configuration -----------------------------------------------------------
config_defaults <- function() list(
  session_name = "",
  experiment_name = "",            # older name for session_name; still accepted
  input = list(mode = "auto", path = "", lif_reader = "direct"),
  unattended = FALSE,
  bc = list(fallback = "auto", template = ""),
  output_root = "",
  fiji = list(path = "", memory = "", extra_args = list()),
  inventory = list(
    dump_original_metadata = FALSE,
    processed_name_regex = "(?i)(lng|lightning|thunder|lvcc|deconv)",
    tile_name_regex     = "(?i)(tilescan|mosaic|merged|stitch)"
  ),
  grouping = list(
    key_fields = c("acq_type", "objective_mag", "objective_na",
                   "size_x", "size_y", "pixel_size_um", "size_c"),
    pixel_size_sig_digits = 3,
    laser_tolerance_pct = 0.5
  ),
  naming = list(
    default_name_regex = "^(Series|Image|Region|Position|TileScan|Mark_and_Find)[ _]?\\d+$",
    name_fields_regex  = "",
    parse_names = TRUE,
    name_patterns = default_name_patterns()
  ),
  user_fields = c("experimental_id", "animal_replicate", "section_coverslip", "fov", "notes"),
  previews = list(max_size = 512, large_max_size = 1536, large_threshold_px = 3000),
  render = list(channel_png = "gray", save_stack = FALSE),
  figures = list()                 # see figure_defaults() in R/figures.R
)

read_config <- function(path, input = NULL, input_mode = NULL) {
  require_pkgs(c("yaml", "jsonlite"))
  if (!file.exists(path)) stop("Config file not found: ", path, call. = FALSE)
  cfg <- utils::modifyList(config_defaults(), yaml::read_yaml(path) %||% list())
  cfg$config_path <- norm_path(path)
  if (!is.null(input) && nzchar(input)) {         # run-script override of the input
    cfg$input$path <- input; cfg$input$mode <- input_mode %||% "auto"
  }

  # -- input: the orchestrator's lif / TIF-folder decision point
  if (!nzchar(cfg$input$path %||% "")) stop("config: input.path is empty", call. = FALSE)
  cfg$input$path <- norm_path(cfg$input$path)
  if (!file.exists(cfg$input$path)) stop("Input not found: ", cfg$input$path, call. = FALSE)
  mode <- cfg$input$mode %||% "auto"
  if (mode == "auto") {
    d <- detect_input(cfg$input$path)
    cfg$input$path <- d$path; mode <- d$mode; cfg$input$extensions <- d$extensions
    if (!is.null(d$note)) msg(d$note)
  }
  is_dir <- dir.exists(cfg$input$path)
  if (mode == "lif" && is_dir) stop("input.mode is 'lif' but input.path is a folder", call. = FALSE)
  if (mode == "tif_folder" && !is_dir) stop("input.mode is 'tif_folder' but input.path is a file", call. = FALSE)
  if (!mode %in% c("lif", "tif_folder", "bioformats"))
    stop("input.mode must be auto, lif, tif_folder or bioformats", call. = FALSE)
  if (mode == "bioformats" && is_dir && is.null(cfg$input$extensions))
    cfg$input$extensions <- detect_input(cfg$input$path)$extensions %||% BF_EXTENSIONS
  cfg$input$mode <- mode

  # -- output: always a NEW folder; raw data is never written to
  base <- if (is_dir) basename(cfg$input$path) else tools::file_path_sans_ext(basename(cfg$input$path))
  # .lif and TIF-export runs of the same session get separate folders
  suffix <- if (mode == "tif_folder") "_tifexport_pipeline" else "_pipeline"
  out  <- cfg$output_root %||% file.path(dirname(cfg$input$path), paste0(base, suffix))
  out  <- norm_path(out)
  if (identical(out, cfg$input$path)) stop("output_root cannot be the input itself", call. = FALSE)
  cfg$session_name <- cfg$session_name %||% cfg$experiment_name %||% base
  cfg$experiment_name <- cfg$session_name

  cfg$paths <- list(
    root          = out,
    manifest      = file.path(out, "manifest"),
    raw_inventory = file.path(out, "manifest", "raw_inventory"),
    history       = file.path(out, "manifest", "history"),
    logs          = file.path(out, "logs")
  )
  for (p in cfg$paths) dir.create(p, recursive = TRUE, showWarnings = FALSE)
  cfg
}

# ---- run log (JSON) ----------------------------------------------------------
write_run_log <- function(cfg, module, details) {
  log <- list(
    module     = module,
    timestamp  = format(Sys.time(), "%Y-%m-%dT%H:%M:%S%z"),
    session    = cfg$session_name,
    input      = cfg$input,
    config     = cfg[setdiff(names(cfg), c("paths"))],
    pipeline_version = tryCatch(readLines(file.path(get0(".pipeline_dir", ifnotfound = "."), "VERSION"),
                                          warn = FALSE)[1], error = function(e) NA),
    r_version  = R.version.string,
    platform   = R.version$platform,
    details    = details
  )
  path <- file.path(cfg$paths$logs, sprintf("%s_%s.json", module, timestamp_id()))
  jsonlite::write_json(log, path, auto_unbox = TRUE, pretty = TRUE, null = "null", na = "null")
  invisible(path)
}

# Make a config for a new session from the pipeline's template (comments kept)
new_session_config <- function(config_path, input, pipeline_dir) {
  if (is.null(input) || !nzchar(input)) stop("Config not found and no input given to create one: ", config_path, call. = FALSE)
  tpl <- readLines(file.path(pipeline_dir, "config.yml"), warn = FALSE)
  i <- grep("^  path: ", tpl)[1]
  tpl[i] <- sprintf("  path: '%s'", gsub("'", "''", norm_path(input)))
  dir.create(dirname(config_path), recursive = TRUE, showWarnings = FALSE)
  writeLines(tpl, config_path)
  msg("Created ", config_path, " from the template (edit it to change options).")
}

# ---- input detection ---------------------------------------------------------------
# Formats read through Bio-Formats in "bioformats" mode (one or more images per file)
BF_EXTENSIONS <- c("czi", "nd2", "oir", "oib", "oif", "ims", "lsm", "vsi", "zvi", "lei", "dv",
                   "ome.tif", "ome.tiff", "ome.zarr")

# A file -> lif (if .lif) or bioformats. A folder -> its single .lif, else its Zeiss/Nikon/...
# files, else LAS X TIF exports, else any TIFs (read with Bio-Formats).
detect_input <- function(path) {
  path <- norm_path(path)
  if (!dir.exists(path)) {
    is_lif <- grepl("\\.lif$", path, ignore.case = TRUE)
    return(list(path = path, mode = if (is_lif) "lif" else "bioformats", extensions = NULL, note = NULL))
  }
  f <- list.files(path); fl <- tolower(f)
  lifs <- f[endsWith(fl, ".lif")]
  if (length(lifs) == 1)
    return(list(path = file.path(path, lifs), mode = "lif", extensions = NULL,
                note = paste("Found", lifs, "in the folder - reading it.")))
  if (length(lifs) > 1)
    stop("The folder has several .lif files (", paste(lifs, collapse = ", "),
         "). Point the pipeline at one of them.", call. = FALSE)
  ext <- BF_EXTENSIONS[vapply(BF_EXTENSIONS, function(e) any(endsWith(fl, paste0(".", e))), TRUE)]
  if (length(ext))
    return(list(path = path, mode = "bioformats", extensions = ext,
                note = sprintf("Reading %d %s file(s) with Bio-Formats.",
                               sum(vapply(fl, function(x) any(endsWith(x, paste0(".", ext))), TRUE)),
                               paste(toupper(ext), collapse = "/"))))
  tifs <- f[grepl("\\.tiff?$", fl)]
  if (length(tifs) && any(parse_leica_tif_names(tifs)$role %in% c("channel", "overlay")))
    return(list(path = path, mode = "tif_folder", extensions = NULL, note = NULL))
  if (length(tifs))
    return(list(path = path, mode = "bioformats", extensions = c("tif", "tiff"), note = NULL))
  stop("No supported image files found in ", path, call. = FALSE)
}

# ---- pre-filling your fields from image/file names ----------------------------------
# Each field has a list of patterns; every pattern that matches adds its <value> (joined with
# a space, e.g. "(+)BAF DIV1"). Nothing is required: names without such information simply
# leave the field blank. Pre-filled values are shown in yellow in the review app to confirm.
default_name_patterns <- function() list(
  experimental_id = c(
    "(?<![A-Za-z0-9])(?<value>\\([+-]\\)\\s?[A-Za-z][A-Za-z0-9]*)",                 # (+)BAF, (-)NGF
    "(?:^|[_ /])(?<value>[+-][A-Za-z][A-Za-z0-9]*)(?=$|[_ /-])",                   # +BAF, -NGF
    paste0("(?i)(?<![A-Za-z])(?<value>WT|KO|cKO|Het|Hom|Ctrl|Control|Vehicle|Veh|Sham|Mock|",
           "Scr|Scrambled|NT|DMSO|PBS|Untreated)(?![A-Za-z])"),                    # genotype / control
    "(?i)(?<![A-Za-z])(?<value>(?:DIV|DPI|DPF|HPF|PND)\\s?\\d+(?:\\.\\d+)?)(?![A-Za-z0-9])"),  # DIV7, DPI3
  animal_replicate = "(?i)(?<![A-Za-z])(?<value>(?:animal|mouse|rat|rep|replicate|exp|expt|batch)[ _-]?\\d+)(?![A-Za-z0-9])",
  section_coverslip = "(?i)(?<![A-Za-z])(?<value>(?:slide|coverslip|cs|section|sec|well|dish)[ _-]?\\d+(?:-\\d+)?)(?![A-Za-z0-9])",
  fov = c(
    "(?i)(?<![A-Za-z])(?<value>(?:fov|field|pos|position|roi|region)[ _-]?\\d+)(?![A-Za-z0-9])",  # FoV2, pos3
    "(?i)(?<![A-Za-z0-9])\\d{2,3}x-(?<value>\\d{1,3})-")                         # Zen "20X-02-"
)

parse_name_fields <- function(names, patterns, fields) {
  out <- as.data.frame(stats::setNames(rep(list(rep(NA_character_, length(names))), length(fields)), fields),
                       stringsAsFactors = FALSE)
  for (f in intersect(names(patterns), fields)) {
    vals <- rep(list(character()), length(names))
    for (p in unlist(patterns[[f]])) {
      v <- tryCatch(regex_named(names, p)$value, error = function(e) {
        warning("Name pattern for '", f, "' is not a valid regular expression: ", p, call. = FALSE); NULL })
      if (is.null(v)) next
      for (i in which(!is.na(v) & nzchar(v))) vals[[i]] <- unique(c(vals[[i]], trimws(v[i])))
    }
    out[[f]] <- vapply(vals, function(x) if (length(x)) paste(x, collapse = " ") else NA_character_, "")
  }
  out
}
