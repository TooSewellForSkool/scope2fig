# tools/build_release.R — build the shareable zip for a GitHub release (maintainer only)
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-13. History: CHANGELOG.md; background: DEVELOPMENT.md.
#
# In R, from the scope2fig folder:   source("tools/build_release.R")
# Writes dist/scope2fig-<version>.zip containing a folder called "scope2fig" with:
#   - everything users need (not .git, .github, tools/, dist/, local_settings.yml)
#   - the user guides as plain .txt (GETTING_STARTED.txt, GUIDE_FOR_THE_UNINITIATED.txt),
#     converted from the .md files, so they open in any text editor instead of RStudio
#   - the Mac launcher marked as executable
# Needs a `zip` command (built into macOS; on Windows install Rtools).

# Markdown -> readable plain text (headings underlined, emphasis/backticks/links simplified)
md_to_txt <- function(lines) {
  out <- character(); in_code <- FALSE
  underline <- function(txt, ch) strrep(ch, max(3, nchar(txt, type = "width")))
  for (ln in lines) {
    if (grepl("^\\s*```", ln)) { in_code <- !in_code; next }
    if (in_code) { out <- c(out, ln); next }                     # commands stay exactly as written
    if (grepl("^---+\\s*$", ln)) { out <- c(out, "", strrep("_", 70), ""); next }
    ln <- gsub("\\*\\*([^*]+)\\*\\*", "\\1", ln)                    # **bold**
    ln <- gsub("(^|[^*\\w])\\*([^* ][^*]*)\\*", "\\1\\2", ln, perl = TRUE)   # *italic*
    ln <- gsub("`([^`]*)`", "\\1", ln)                              # `code`
    ln <- gsub("\\[([^]]+)\\]\\((#[^)]*|[^)]*\\.md)\\)", "\\1", ln)    # internal links -> text
    ln <- gsub("\\[([^]]+)\\]\\(([^)]+)\\)", "\\1 (\\2)", ln)          # external links -> text (url)
    ln <- sub("^> ?", "  ", ln)                                     # quotes -> indented
    if (grepl("^# ", ln))   { t <- toupper(sub("^# +", "", ln)); out <- c(out, t, underline(t, "=")); next }
    if (grepl("^## ", ln))  { t <- toupper(sub("^## +", "", ln)); out <- c(out, t, underline(t, "-")); next }
    if (grepl("^### ", ln)) { t <- sub("^### +", "", ln); out <- c(out, t, underline(t, "~")); next }
    out <- c(out, ln)
  }
  out
}

build_release <- function(root = getwd()) {
  root <- normalizePath(root)
  if (!file.exists(file.path(root, "VERSION"))) stop("Run this from the scope2fig folder.", call. = FALSE)
  ver <- trimws(readLines(file.path(root, "VERSION"), warn = FALSE)[1])
  dist <- file.path(root, "dist"); stage <- file.path(dist, "stage"); dest <- file.path(stage, "scope2fig")
  unlink(stage, recursive = TRUE); dir.create(dest, recursive = TRUE)

  files <- list.files(root, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  skip <- "^(dist|tools|\\.git|\\.github|\\.Rproj\\.user)(/|$)|(^|/)(local_settings\\.yml|\\.DS_Store|\\.Rhistory|\\.RData|\\.gitignore)$"
  files <- files[!grepl(skip, files)]
  for (f in files) {
    dir.create(dirname(file.path(dest, f)), recursive = TRUE, showWarnings = FALSE)
    file.copy(file.path(root, f), file.path(dest, f), copy.mode = TRUE)
  }
  for (g in c("GETTING_STARTED", "GUIDE_FOR_THE_UNINITIATED")) {
    md <- file.path(dest, paste0(g, ".md"))
    if (file.exists(md)) { writeLines(md_to_txt(readLines(md, warn = FALSE, encoding = "UTF-8")),
                                      file.path(dest, paste0(g, ".txt")), useBytes = TRUE); unlink(md) }
  }
  Sys.chmod(file.path(dest, "launch", "Start Pipeline (Mac).command"), "755")

  zipfile <- file.path(dist, paste0("scope2fig-", ver, ".zip")); unlink(zipfile)
  old <- setwd(stage); on.exit(setwd(old), add = TRUE)
  status <- utils::zip(zipfile, "scope2fig", flags = "-r9Xq")
  setwd(old); unlink(stage, recursive = TRUE)
  if (!identical(as.integer(status), 0L) || !file.exists(zipfile)) stop("zip failed (is a 'zip' command available?)", call. = FALSE)
  message("Release zip: ", zipfile, "  (", length(files), " files)")
  invisible(zipfile)
}

build_release()
