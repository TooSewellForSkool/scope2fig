# fiji.R — find Fiji on any OS and run a Fiji script headless from R
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.

fiji_launcher_candidates <- function() {
  sys <- Sys.info()[["sysname"]]
  home <- path.expand("~")
  apps <- switch(sys,
    Darwin  = c("/Applications/Fiji.app", file.path(home, "Applications/Fiji.app"),
                file.path(home, "Desktop/Fiji.app")),
    Windows = c("C:/Fiji.app", file.path(home, "Fiji.app"), file.path(home, "Desktop/Fiji.app"),
                "C:/Program Files/Fiji.app"),
    c(file.path(home, "Fiji.app"), "/opt/Fiji.app", file.path(home, "Desktop/Fiji.app"))
  )
  # Older ImageJ-* launchers and the newer fiji-* launchers
  exes <- switch(sys,
    Darwin  = c("Contents/MacOS/ImageJ-macosx", "Contents/MacOS/fiji-macos",
                "fiji-macos-x64", "fiji-macos-arm64", "fiji-macos"),
    Windows = c("ImageJ-win64.exe", "fiji-windows-x64.exe", "fiji-windows-arm64.exe"),
    c("ImageJ-linux64", "fiji-linux-x64", "fiji-linux-arm64")
  )
  list(apps = apps, exes = exes)
}

find_fiji <- function(cfg) {
  cand <- fiji_launcher_candidates()
  user <- cfg$fiji$path %||% ""
  if (!nzchar(user) && exists("read_local_settings")) user <- read_local_settings()$fiji_path %||% ""   # launcher remembers it
  if (nzchar(user)) {
    user <- norm_path(user)
    if (file.exists(user) && !dir.exists(user)) return(user)        # pointed at the launcher
    if (dir.exists(user)) cand$apps <- user                            # pointed at Fiji.app
    else stop("fiji.path does not exist: ", user, call. = FALSE)
  }
  for (app in cand$apps) for (exe in cand$exes) {
    p <- file.path(app, exe)
    if (file.exists(p)) return(norm_path(p))
  }
  stop("Could not find Fiji. Set fiji.path in config.yml to your Fiji.app folder ",
       "or to the launcher inside it.", call. = FALSE)
}

# params: named list -> SciJava script-parameter string: a="x",b=true
fiji_param_string <- function(params) {
  vals <- vapply(params, function(v) {
    if (is.logical(v)) tolower(as.character(v))
    else sprintf('"%s"', gsub('"', '\\\\"', as.character(v)))
  }, character(1))
  paste(sprintf("%s=%s", names(params), vals), collapse = ",")
}

run_fiji_script <- function(cfg, script, params, log_file, expect_file = NULL) {
  fiji <- find_fiji(cfg)
  args <- c(if (nzchar(cfg$fiji$memory %||% "")) paste0("--mem=", cfg$fiji$memory),
            unlist(cfg$fiji$extra_args),
            "--headless", "--console", "--run", norm_path(script), fiji_param_string(params))
  qtype <- if (.Platform$OS.type == "windows") "cmd" else "sh"
  msg("Running Fiji headless: ", basename(script))
  # system2() already quotes the command itself; only the arguments need quoting
  status <- system2(fiji, shQuote(args, type = qtype),
                    stdout = log_file, stderr = log_file)
  log_txt <- if (file.exists(log_file)) readLines(log_file, warn = FALSE) else character()
  failed <- (!is.null(status) && status != 0) ||
            (!is.null(expect_file) && !file.exists(expect_file))
  if (failed) {
    stop("Fiji script failed (exit ", status, "). Last lines of ", log_file, ":\n",
         paste(utils::tail(log_txt, 25), collapse = "\n"), call. = FALSE)
  }
  invisible(list(fiji = fiji, status = status, log = log_txt))
}
