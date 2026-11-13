# run_session.R — copy this file anywhere you like (e.g. next to your data or in your
# Part of scope2fig (version: see VERSION) - Jonathon Sewell, developed with Claude
# (Anthropic). Created 2026-11-05. History: CHANGELOG.md; background: DEVELOPMENT.md.
# scripts folder), edit the block below, and Source it. Keep one copy per session.
# ============================== edit me ==============================================
PIPELINE <- "/path/to/scope2fig"          # this folder
SESSION  <- "/path/to/your/imaging/session"       # folder with the .lif, or the .czi/.nd2 files

CONFIG <- file.path(SESSION, "config.yml")   # created from the template if it doesn't exist

# What to read:
INPUT <- SESSION                             # a folder: its .lif, else its .czi/.nd2/... files,
                                             #   else LAS X TIF exports - detected automatically
# INPUT <- file.path(SESSION, "Project.lif") # or one specific file (.lif, .czi, .nd2, ...)
# INPUT <- file.path(SESSION, "Project")     # or a LAS X export folder (TIF fallback)
# INPUT <- NULL                              # use input.path from config.yml

STEPS <- "all"            # "all", or e.g. "inventory", c("render", "figures")

INTERACTIVE <- TRUE       # TRUE  = open the review and B&C apps
                          # FALSE = set-and-forget: use saved review/B&C settings; groups without
                          #         settings follow `bc:` in config.yml (auto, template or skip)
# =====================================================================================

source(file.path(PIPELINE, "run_pipeline.R"))
run_pipeline(CONFIG, steps = STEPS, input = INPUT, interactive = INTERACTIVE)

# Optional: check the built-in .lif reader against LAS X exports of the same images
# validate_lif_reader(CONFIG, export_folder = file.path(SESSION, "Project"))
