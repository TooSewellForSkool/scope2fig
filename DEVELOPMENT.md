# About scope2fig

| | |
|---|---|
| **Name** | scope2fig (formerly "confocal_pipeline"; renamed 2026-11-06 once it read any microscope format) |
| **Repository** | https://github.com/TooSewellForSkool/scope2fig |
| **License** | GNU General Public License v3.0 (`LICENSE`) |
| **Version** | see `VERSION` (history in `CHANGELOG.md`) |
| **Author** | Jonathon Sewell - design, original Fiji macros and R scripts, testing on real data |
| **Code** | Written with Claude (Anthropic) in working sessions with the author |
| **Created** | 2026-11-05 |
| **Last updated** | 2026-11-13 |

Please acknowledge both when the tool or its figures are used (e.g. "Images were processed
with a custom R/Fiji pipeline (J. Sewell), developed with the assistance of Claude, Anthropic").

## Why it exists

The starting point was a manual workflow on a Leica confocal: export every image from LAS X as
one TIF per channel, rebuild hyperstacks with ImageJ macros (`Images_to_Hyperstack.ijm`,
`Series_to_Hyperstack_v1.ijm`), apply brightness/contrast typed into a macro
(`Image_4ChannelMerge.ijm`, `Series_MaxZProj_4ChannelMerge.ijm`; values recorded only in the
output folder name), then assemble montages by hand in R (`magick`, several dataset-specific
.Rmd scripts) or in PowerPoint. The goals were one entry point, no manual export, reproducible
documented settings, and figures for deciding what to quantify.

## Design decisions

- **Raw data are never modified.** All outputs go to a new `<name>_pipeline/` folder.
- **Linear display only.** Brightness/contrast is min/max (Fiji `setMinAndMax`), no gamma or
  per-image auto-contrast, and it is shared by every image in a group. `_composite.tif` keeps
  original pixel values; PNGs/figures are for viewing.
- **Hierarchy:** session (one .lif or folder) > experiment (staining panel; owns channel names
  and colors) > group (comparably acquired images: same acquisition type, objective, image size,
  pixel size, channels; laser % / exposure checked within a group).
- **Everything is recorded:** manifest of every image and its acquisition settings, channel
  map, B&C settings per group (with history), JSON run logs, settings copied next to renders,
  B&C values printed on every PowerPoint slide.
- **R is the front door, Fiji the image engine.** Fiji runs headless (Groovy scripts) for reading
  and rendering; R handles configuration, apps (Shiny), tables and figures (`magick`, `officer`).
- **Matching channels by name, not position**, which fixed a color mix-up in the original macros.

## Notable problems found and solved

- **Original macro bugs** (fixed by the redesign): first file in each folder skipped (`i=1`),
  `.tif` filter disabled by a stray `;`, channel colors inconsistent between scripts, single
  planes not adjusted in the z-projection macro, z-planes >= 10 misnamed (`_z010`).
- **Laser metadata:** Bio-Formats flattens Leica's XML and drops most laser lines, so laser %,
  detectors, gains, zoom and tiles are read from Leica's own XML in the .lif header.
- **Bio-Formats 8.1.1 misread some .lif series:** 5 images showed the *next* image's data,
  shifted, with a wrapped strip at the left edge (confirmed against LAS X exports). The pipeline
  now reads .lif pixels itself: it finds each image's memory block by its ID and uses Leica's byte
  layout. Validated as bit-identical to LAS X exports for 12 images x 4 channels
  (`validate_lif_reader()`). Previews warn whenever two images have identical statistics.
- **Tile scans:** individual tiles are recognized and excluded when a stitched image exists.
- **Photon-counting data** (small integer ranges) needed B&C controls with typed values.

## How it is organized

| Path | Role |
|---|---|
| `run_pipeline.R` | entry point; runs the steps; wraps them in error reporting |
| `run_session_TEMPLATE.R` | per-session run script for R users |
| `launch/` | double-click launchers (Mac `.command`, Windows `.bat`) and `launch.R` |
| `README.md`, `GETTING_STARTED.md`, `GUIDE_FOR_THE_UNINITIATED.md` | landing page; short setup; step-by-step setup for beginners |
| `tools/build_release.R` | builds `dist/scope2fig-<version>.zip` (guides as .txt) for GitHub releases |
| `.github/ISSUE_TEMPLATE/` | bug-report form for GitHub Issues |
| `config.yml` | template for each session's settings |
| `support.yml` | who users contact about errors (shown with the error report) |
| `R/utils.R`, `R/fiji.R` | config, input detection, logging; finding and running Fiji |
| `R/inventory.R`, `fiji/inventory.groovy` | step 1: list images and settings, auto-group |
| `R/leica_xml.R`, `R/lif_reader.R` | Leica XML metadata; built-in .lif pixel reader + validation |
| `R/vendor_meta.R` | Zeiss .czi metadata; generic OME metadata for other formats |
| `R/parse_tif_names.R` | LAS X TIF export names (fallback input) |
| `R/previews.R`, `fiji/previews.groovy` | thumbnails and small raw previews |
| `R/review.R`, `R/review_app.R` | step 2: review app |
| `R/tune.R`, `R/tune_app.R` | step 3: B&C app and unattended settings |
| `R/render.R`, `fiji/render.groovy` | step 4: full-resolution outputs |
| `R/figures.R` | step 5: montages, grids, PowerPoint |
| `R/error_report.R`, `R/launcher.R` | error reports; launcher dialogs, package install, Fiji lookup |
| `tests/` | parser and Leica XML checks |

## Tested on

- Early users (Nov 2026): setup from the zip was the main hurdle (Terminal, macOS unblocking,
  folder access), which led to the Guide for the uninitiated.
- Leica Stellaris (LAS X) `.lif`: single planes, z-stacks, tile scan (29 series), 16-bit
  photon counting, macOS (Intel), R 4.5.2, Fiji with Bio-Formats 8.1.1.
- Zeiss Axio Observer + Apotome `.czi` (6 files, 4 channels, single plane).

## Known limitations / ideas

- Nikon `.nd2` and other formats use generic Bio-Formats metadata; a Nikon metadata reader
  should be added once sample files are available.
- The built-in .lif reader is validated for single planes; z-stack stepping is not yet checked
  against raw LAS X exports.
- Tile pairing (tiles vs stitched image) is implemented for Leica only.
- Possible next steps: quantification module reading the manifest and composite TIFs; packaging
  as an R package.
