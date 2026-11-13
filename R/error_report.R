# error_report.R — when a step fails, write one plain-text report the user can send on.
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# The report holds what is needed to diagnose a problem without access to the computer:
# the error and where it happened, warnings, versions (pipeline, R, OS, packages, Fiji),
# the config, a summary of the manifest and the end of the most recent Fiji logs.
# No image data is included (file names and paths are).

support_info <- function() {
  f <- file.path(get0(".pipeline_dir", ifnotfound = getwd()), "support.yml")
  s <- if (file.exists(f) && requireNamespace("yaml", quietly = TRUE)) tryCatch(yaml::read_yaml(f), error = function(e) list()) else list()
  utils::modifyList(list(contact_name = "the person who shared this tool", contact_email = "",
                         issues_url = "", message = ""), s %||% list())
}

support_message <- function(report_path) {
  s <- support_info()
  who <- paste0(s$contact_name, if (nzchar(s$contact_email)) paste0(" (", s$contact_email, ")") else "")
  paste0("An error report was saved to:\n  ", report_path, "\n",
         "Please send that file to ", who, ".",
         if (nzchar(s$issues_url)) paste0("\nOr attach it to a new issue at ", s$issues_url) else "",
         if (nzchar(s$message)) paste0("\n", s$message) else "")
}

# Run `f()`, recording warnings; on error write a report, then re-raise the error with
# the report path attached (so callers such as the launcher can show it).
# `state` is an environment the caller keeps up to date: config, steps, input, apps, cfg,
# current_step.
with_error_report <- function(f, state = new.env()) {
  calls <- NULL; warns <- character(); ctx <- environment()
  tryCatch(
    withCallingHandlers(f(),
      warning = function(w) assign("warns", c(get("warns", ctx), conditionMessage(w)), envir = ctx),
      error = function(e) if (is.null(get("calls", ctx))) assign("calls", sys.calls(), envir = ctx)),
    error = function(e) {
      if (!is.null(e$report)) stop(e)                          # already reported further down
      step <- state$current_step %||% NA
      path <- tryCatch(write_error_report(e, calls, warns, state, step),
                       error = function(e2) { message("(Could not write an error report: ", conditionMessage(e2), ")"); NULL })
      if (!is.null(path)) {
        message("\n", strrep("=", 70), "\nThe pipeline stopped with an error", if (!is.na(step)) paste0(" in step '", step, "'"),
                ":\n  ", conditionMessage(e), "\n\n", support_message(path), "\n", strrep("=", 70))
      }
      e$report <- path
      stop(e)
    })
}

write_error_report <- function(e, calls, warns, state, step) {
  cfg <- state$cfg; context <- state
  ts <- format(Sys.time(), "%Y%m%d-%H%M%S")
  dir <- if (!is.null(cfg$paths$logs)) cfg$paths$logs
         else if (!is.null(context$config)) dirname(norm_path(context$config))
         else file.path(get0(".pipeline_dir", ifnotfound = getwd()), "error_reports")
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  path <- file.path(dir, paste0("error_report_", ts, ".txt"))
  L <- character(); add <- function(...) L <<- c(L, paste0(...)); sec <- function(t) add("\n## ", t, "\n")
  safe <- function(expr) tryCatch(expr, error = function(x) paste("<unavailable:", conditionMessage(x), ">"))

  ver <- safe(readLines(file.path(get0(".pipeline_dir", ifnotfound = "."), "VERSION"), warn = FALSE)[1])
  add("SCOPE2FIG - ERROR REPORT")
  add("Created:  ", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z"))
  add("Pipeline: ", ver, "   (", get0(".pipeline_dir", ifnotfound = "?"), ")")
  add("Step:     ", if (is.na(step)) "(before any step / setup)" else step)
  add("Error:    ", conditionMessage(e))
  add("Contains no image data. It does contain file names, folder paths and settings.")

  sec("What was run")
  add("config:      ", context$config %||% "?")
  add("steps:       ", paste(context$steps %||% "?", collapse = ", "))
  add("apps:        ", context$apps %||% "?")
  add("input:       ", safe(cfg$input$path %||% context$input %||% "?"), "  (mode: ", safe(cfg$input$mode %||% "?"), ")")
  add("output:      ", safe(cfg$paths$root %||% "?"))

  sec("Where it happened (most recent call last)")
  if (length(calls)) {
    txt <- vapply(calls, function(cl) substr(paste(deparse(cl, nlines = 1), collapse = ""), 1, 160), "")
    txt <- txt[!grepl("^(withCallingHandlers|tryCatch|tryCatchList|tryCatchOne|doTryCatch|with_error_report|h\\(|stop\\(|\\.handleSimpleError)", txt)]
    add(paste0("  ", utils::tail(txt, 25)))
  } else add("  (no call stack recorded)")

  sec("Warnings during this run")
  if (length(warns)) add(paste0("  - ", utils::tail(unique(warns), 30))) else add("  (none)")

  sec("System")
  add("R:        ", R.version.string, " on ", R.version$platform)
  si <- Sys.info(); add("OS:       ", si[["sysname"]], " ", si[["release"]], " (", si[["machine"]], ")")
  add("Fiji:     ", safe(find_fiji(cfg %||% list(fiji = list(path = "")))))
  pk <- vapply(PIPELINE_PACKAGES, function(p) tryCatch(as.character(utils::packageVersion(p)), error = function(x) "NOT INSTALLED"), "")
  add("Packages: ", paste(paste0(names(pk), " ", pk), collapse = ", "))

  sec("Config file")
  cp <- context$config
  if (!is.null(cp) && file.exists(cp)) {
    y <- readLines(cp, warn = FALSE); y <- y[!grepl("^\\s*#", y) & nzchar(trimws(y))]
    add(paste0("  ", y))
  } else add("  (no config file)")

  sec("Manifest summary")
  mp <- if (!is.null(cfg$paths$manifest)) file.path(cfg$paths$manifest, "image_manifest.csv") else ""
  if (nzchar(mp) && file.exists(mp)) {
    m <- safe(read_csv_safe(mp))
    if (is.data.frame(m)) {
      add("  images: ", nrow(m), ", included: ", sum(as.logical(m$include) %in% TRUE))
      g <- table(paste(m$experiment %||% "", m$group %||% "", sep = " / ")[as.logical(m$include) %in% TRUE])
      for (k in names(g)) add("  ", k, ": ", g[[k]])
      fl <- m$flags[!is.na(m$flags) & m$flags != ""]
      if (length(fl)) add("  flags: ", paste(utils::head(unique(fl), 10), collapse = " | "))
    } else add("  ", m)
  } else add("  (no manifest yet)")

  sec("End of the most recent Fiji logs")
  ld <- cfg$paths$logs %||% ""
  logs <- if (nzchar(ld)) list.files(ld, pattern = "^fiji_.*\\.log$", full.names = TRUE) else character()
  if (length(logs)) {
    kinds <- sub("_\\d{8}-\\d{6}\\.log$", "", basename(logs))
    newest <- vapply(split(logs, kinds), function(v) v[which.max(file.mtime(v))], "")
    for (f in newest[order(file.mtime(newest), decreasing = TRUE)][1:min(3, length(newest))]) {
      add("--- ", basename(f), " ---")
      x <- readLines(f, warn = FALSE)
      x <- x[!grepl("Unknown (Immersion|Correction) value|Unable to locate a Java Runtime|java\\.com for information", x)]
      add(paste0("  ", utils::tail(x, 40)))
    }
  } else add("  (no Fiji logs)")

  sec("Recent run logs")
  if (nzchar(ld)) {
    j <- list.files(ld, pattern = "\\.json$", full.names = TRUE)
    if (length(j)) add(paste0("  ", basename(utils::tail(j[order(file.mtime(j))], 8)))) else add("  (none)")
  }
  writeLines(L, path, useBytes = TRUE)
  path
}
