# run_pipeline.R — the pipeline's entry point. You normally don't edit this file:
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# copy run_session_TEMPLATE.R, set the paths at its top, and run that instead.
#
#   source("/path/to/scope2fig/run_pipeline.R")
#   run_pipeline(config, steps = "all", input = NULL, interactive = TRUE)
#
#   config       path to the session's config.yml (created from the template if missing
#                and `input` is given)
#   steps        "all", or any of the steps below, e.g. c("render", "figures")
#   input        optional: a .lif file or a LAS X export folder. Overrides input.path in
#                the config; the mode follows from it (file = .lif, folder = TIF exports).
#                .lif and TIF-export runs write to separate output folders.
#   interactive  TRUE = open the review and B&C apps; FALSE = never open them (uses your
#                saved review/settings; groups without settings follow config `bc:`).
#                Default: the config's `unattended:` setting.
# From a terminal:  Rscript run_pipeline.R /path/to/config.yml   (runs unattended)
#
# Steps:
#   inventory  read .lif / TIF folder -> manifest/image_manifest.csv
#   review     thumbnails + app to confirm experiments, groups, channel names, your fields
#   tune       B&C per experiment/group -> settings/*.json
#   render     full-res projections, PNGs, composite TIFs via Fiji
#   figures    montages, comparison grids, PowerPoint

# The script Rscript was started with (Rscript writes spaces in its path as "~+~")
.rscript_file <- function() {
  f <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(f)) gsub("~+~", " ", sub("^--file=", "", f[1]), fixed = TRUE) else character()
}

.pipeline_dir <- local({
  cand <- c(tryCatch(dirname(normalizePath(sys.frame(1)$ofile)), error = function(e) NULL),   # source()
            getOption("scope2fig_dir"),                                              # launcher
            { f <- .rscript_file()                                                           # Rscript
              if (length(f)) dirname(normalizePath(f, mustWork = FALSE)) },
            getwd())
  ok <- Filter(function(d) file.exists(file.path(d, "R", "inventory.R")), cand)
  if (length(ok)) ok[[1]] else stop("Could not find the pipeline's R/ folder.")
})

for (.f in list.files(file.path(.pipeline_dir, "R"), pattern = "\\.R$", full.names = TRUE)) source(.f)

PIPELINE_STEPS <- c("inventory", "review", "tune", "render", "figures")

run_pipeline <- function(config, steps = "all", input = NULL, interactive = NULL,
                         input_mode = NULL, pipeline_dir = .pipeline_dir) {
  # everything runs inside with_error_report(): on failure an error report is written
  # (logs/error_report_<time>.txt) and the user is told where it is and whom to send it to
  state <- new.env()
  state$config <- config; state$steps <- steps; state$input <- input; state$current_step <- NA
  with_error_report(function() .run_pipeline_steps(state, config, steps, input, interactive,
                                                   input_mode, pipeline_dir), state)
}

.run_pipeline_steps <- function(state, config, steps, input, interactive, input_mode, pipeline_dir) {
  if (!file.exists(config)) new_session_config(config, input, pipeline_dir)
  cfg <- read_config(config, input = input, input_mode = input_mode)
  state$cfg <- cfg
  if (identical(steps, "all")) steps <- PIPELINE_STEPS
  bad <- setdiff(steps, PIPELINE_STEPS)
  if (length(bad)) stop("Unknown step(s): ", paste(bad, collapse = ", "),
                        ". Use 'all' or: ", paste(PIPELINE_STEPS, collapse = ", "), call. = FALSE)
  apps <- if (!is.null(interactive)) isTRUE(interactive) else !isTRUE(cfg$unattended) && base::interactive()
  state$apps <- apps
  msg("Session: ", cfg$session_name, "  |  input: ", cfg$input$mode,
      if (cfg$input$mode == "lif") paste0(" (pixel reader: ", pixel_reader_tag(cfg), ")"),
      "  |  apps: ", if (apps) "on" else "off (unattended)")
  msg("Output folder: ", cfg$paths$root)
  out <- list()
  for (st in PIPELINE_STEPS[PIPELINE_STEPS %in% steps]) {
    msg("---- step: ", st, " ----"); state$current_step <- st
    out[[st]] <- switch(st,
      inventory = run_inventory(cfg, pipeline_dir),
      review    = run_review(cfg, pipeline_dir, launch = apps),
      tune      = run_tune(cfg, pipeline_dir, launch = apps),
      render    = run_render(cfg, pipeline_dir),
      figures   = run_figures(cfg, pipeline_dir))
  }
  msg("Done.")
  invisible(out)
}

# Only when this file itself is run from a terminal (not when the launcher sources it)
if (!interactive() && length(commandArgs(TRUE)) && identical(basename(.rscript_file()), "run_pipeline.R"))
  run_pipeline(commandArgs(TRUE)[1], interactive = FALSE)
