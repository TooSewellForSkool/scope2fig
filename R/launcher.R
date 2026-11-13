# launcher.R — double-click entry (launch/Start Pipeline.command / .bat call launch/launch.R)
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# Asks for an image folder and what to do, installs missing R packages once, finds Fiji
# (asks once and remembers), then runs the pipeline. Uses only base R until packages exist.

PIPELINE_PACKAGES <- c("yaml", "jsonlite", "xml2", "shiny", "DT", "tiff", "png", "magick", "officer")

ensure_packages <- function(pkgs = PIPELINE_PACKAGES) {
  missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
  if (!length(missing)) return(invisible(TRUE))
  message("First run: installing R packages (", paste(missing, collapse = ", "), "). This takes a few minutes...")
  utils::install.packages(missing, repos = "https://cloud.r-project.org")
  still <- missing[!vapply(missing, requireNamespace, logical(1), quietly = TRUE)]
  if (length(still)) stop("Could not install: ", paste(still, collapse = ", "),
                          ". Check the internet connection, or install them in RStudio.", call. = FALSE)
  invisible(TRUE)
}

# ---- remembered per-computer settings (not shared; ignored by git) --------------------
local_settings_file <- function() file.path(get0(".pipeline_dir", ifnotfound = getwd()), "local_settings.yml")
read_local_settings <- function() {
  f <- local_settings_file()
  if (file.exists(f) && requireNamespace("yaml", quietly = TRUE)) yaml::read_yaml(f) %||% list() else list()
}
write_local_settings <- function(x) {
  if (!requireNamespace("yaml", quietly = TRUE)) return(invisible())
  yaml::write_yaml(x, local_settings_file())
}

# ---- native dialogs ---------------------------------------------------------------------
os_name <- function() Sys.info()[["sysname"]]

run_dialog <- function(cmd, args) {
  out <- suppressWarnings(tryCatch(system2(cmd, args, stdout = TRUE, stderr = FALSE), error = function(e) character()))
  if (!length(out) || !is.null(attr(out, "status")) || !nzchar(out[1]) || out[1] == "false") NULL else trimws(out[1])
}

ask_line <- function(prompt) {
  cat(prompt)
  if (interactive()) readline() else readLines(file("stdin"), n = 1)
}

choose_folder_dialog <- function(prompt) {
  p <- switch(os_name(),
    Darwin  = run_dialog("osascript", c("-e", shQuote(sprintf('POSIX path of (choose folder with prompt "%s")', prompt)))),
    Windows = run_dialog("powershell", c("-NoProfile", "-STA", "-Command", shQuote(paste0(
      "Add-Type -AssemblyName System.Windows.Forms; $d = New-Object System.Windows.Forms.FolderBrowserDialog; ",
      "$d.Description = '", prompt, "'; if ($d.ShowDialog() -eq 'OK') { $d.SelectedPath }"), type = "cmd"))),
    if (nzchar(Sys.which("zenity"))) run_dialog("zenity", c("--file-selection", "--directory", shQuote(paste0("--title=", prompt))))
    else ask_line(paste0(prompt, "\nFolder path: ")))
  if (is.null(p) || !nzchar(p)) NULL else sub("[/\\\\]$", "", p)
}

choose_file_dialog <- function(prompt, mac_type = NULL, win_filter = "All files (*.*)|*.*") {
  switch(os_name(),
    Darwin  = run_dialog("osascript", c("-e", shQuote(sprintf('POSIX path of (choose file with prompt "%s"%s)', prompt,
                          if (is.null(mac_type)) "" else sprintf(' of type {"%s"}', mac_type))))),
    Windows = run_dialog("powershell", c("-NoProfile", "-STA", "-Command", shQuote(paste0(
      "Add-Type -AssemblyName System.Windows.Forms; $d = New-Object System.Windows.Forms.OpenFileDialog; ",
      "$d.Title = '", prompt, "'; $d.Filter = '", win_filter, "'; if ($d.ShowDialog() -eq 'OK') { $d.FileName }"), type = "cmd"))),
    ask_line(paste0(prompt, "\nPath: ")))
}

LAUNCH_ACTIONS <- c(
  "Full run (open the review and B&C apps)",
  "Full run without apps (use saved settings)",
  "Review images (experiments, groups, names)",
  "Adjust brightness & contrast",
  "Re-make renders and figures only")

choose_action_dialog <- function() {
  if (os_name() == "Darwin") {
    items <- paste0('"', LAUNCH_ACTIONS, '"', collapse = ", ")
    a <- run_dialog("osascript", c("-e", shQuote(sprintf(
      'choose from list {%s} with prompt "What do you want to do?" default items {"%s"}', items, LAUNCH_ACTIONS[1]))))
    return(if (is.null(a)) NULL else match(a, LAUNCH_ACTIONS))
  }
  cat("\nWhat do you want to do?\n"); for (i in seq_along(LAUNCH_ACTIONS)) cat(sprintf("  %d  %s\n", i, LAUNCH_ACTIONS[i]))
  k <- suppressWarnings(as.integer(ask_line("Type a number and press Enter: ")))
  if (is.na(k) || k < 1 || k > length(LAUNCH_ACTIONS)) NULL else k
}

notify <- function(title, text) {
  if (os_name() == "Darwin")
    run_dialog("osascript", c("-e", shQuote(sprintf('display alert "%s" message "%s"', title, gsub('"', "'", text)))))
  else message(title, ": ", text)
}

# Fiji: config -> remembered path -> standard locations -> ask once and remember
ensure_fiji <- function() {
  ok <- tryCatch({ find_fiji(list(fiji = list(path = ""))); TRUE }, error = function(e) FALSE)
  if (ok) return(invisible(TRUE))
  notify("Fiji not found", "Please locate Fiji (Fiji.app). It is only asked once.")
  p <- if (os_name() == "Darwin") choose_file_dialog("Locate Fiji.app", "com.apple.application-bundle")
       else choose_file_dialog("Locate the Fiji program (e.g. ImageJ-win64.exe or fiji-windows-x64.exe)",
                               win_filter = "Fiji (*.exe)|*.exe")
  if (is.null(p)) stop("Fiji is needed. Install it from https://fiji.sc and try again.", call. = FALSE)
  s <- read_local_settings(); s$fiji_path <- sub("/$", "", p); write_local_settings(s)
  find_fiji(list(fiji = list(path = "")))
  invisible(TRUE)
}

launch_pipeline <- function() {
  folder <- choose_folder_dialog("Choose the folder with your images (.lif, .czi, .nd2, or exported TIFs)")
  if (is.null(folder)) return(invisible(message("No folder chosen - nothing to do.")))
  action <- choose_action_dialog()
  if (is.null(action)) return(invisible(message("Cancelled.")))
  ensure_fiji()
  config <- file.path(folder, "config.yml")
  have_manifest <- tryCatch(file.exists(file.path(read_config(config, input = folder)$paths$manifest,
                                                  "image_manifest.csv")), error = function(e) FALSE)
  plan <- switch(action,
    list(steps = "all", apps = TRUE),
    list(steps = "all", apps = FALSE),
    list(steps = c(if (!have_manifest) "inventory", "review"), apps = TRUE),
    list(steps = c(if (!have_manifest) "inventory", "tune"), apps = TRUE),
    list(steps = c("render", "figures"), apps = FALSE))
  message("\nImage folder: ", folder, "\nAction: ", LAUNCH_ACTIONS[action], "\n")
  report <- NULL
  res <- tryCatch({ run_pipeline(config, steps = plan$steps, input = folder, interactive = plan$apps); TRUE },
                  error = function(e) {
                    if (is.null(e$report)) message("\nERROR: ", conditionMessage(e))
                    report <<- e$report; FALSE })
  out <- tryCatch(read_config(config, input = folder)$paths$root, error = function(e) NULL)
  if (res && !is.null(out)) {
    notify("Pipeline finished", paste("Results are in", out))
    if (os_name() == "Darwin") system2("open", shQuote(out)) else if (os_name() == "Windows") shell.exec(out)
  } else if (!res && !is.null(report)) {
    notify("The pipeline stopped with an error", support_message(report))
    if (os_name() == "Darwin") system2("open", c("-R", shQuote(report)))             # show it in Finder
    else if (os_name() == "Windows") system2("explorer", paste0("/select,", shQuote(normalizePath(report), type = "cmd")))
  } else if (!res) notify("The pipeline stopped with an error", "See the message in the Terminal window.")
  invisible(res)
}
