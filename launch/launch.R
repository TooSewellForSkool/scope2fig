# Called by "Start Pipeline (Mac).command" / "Start Pipeline (Windows).bat". Not for editing.
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
args <- commandArgs(TRUE)
PIPE <- normalizePath(args[1])
options(scope2fig_dir = PIPE, repos = c(CRAN = "https://cloud.r-project.org"))
cat("scope2fig ", tryCatch(readLines(file.path(PIPE, "VERSION"), warn = FALSE)[1], error = function(e) ""), "\n", sep = "")
source(file.path(PIPE, "R", "launcher.R"))
ensure_packages()
source(file.path(PIPE, "run_pipeline.R"))
launch_pipeline()
