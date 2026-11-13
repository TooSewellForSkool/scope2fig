# scope2fig

**From any microscope file to documented, comparable figures.**

Point scope2fig at a folder of microscope images (Leica `.lif`, Zeiss `.czi`, Nikon `.nd2`,
and most formats Fiji can open) and it gives you:

- a **montage per image** - every channel plus the merge, labelled, with a scale bar
- **comparison grids** - images side by side, sorted by condition
- a **PowerPoint** with one slide per image (and the display settings printed on each)
- **unaltered composite TIFs** ready for measuring, plus per-channel PNGs
- a record of everything: which images, how they were acquired, which settings were used

Brightness/contrast is set once per group of comparably acquired images (linear min/max only),
so images you compare are displayed the same way. Your original files are never changed.

## Start here

| If you... | Read |
|---|---|
| have never used Terminal or R | **[Guide for the uninitiated](GUIDE_FOR_THE_UNINITIATED.md)** - step by step, with copy-paste commands |
| are comfortable installing software | **[Getting started](GETTING_STARTED.md)** - the short version |
| use R and want scripts / automation | [For R users](#running) below |
| hit a problem | see [error reports](#when-something-goes-wrong-error-reports), or open an [issue](https://github.com/TooSewellForSkool/scope2fig/issues) |

**Download:** the latest zip under **Releases** (right side of this page), or **Code > Download ZIP**.
**Needs:** R (free) and Fiji (free). The launcher installs everything else on first run.

---

Author: Jonathon Sewell, developed with Claude (Anthropic). Version: `VERSION`; history:
[`CHANGELOG.md`](CHANGELOG.md); background and design: [`DEVELOPMENT.md`](DEVELOPMENT.md).

## Inputs (detected automatically from the folder you point at)
| Input | Mode | Pixels read by | Acquisition metadata from |
|---|---|---|---|
| Leica `.lif` | `lif` | built-in .lif reader (validated vs LAS X) | Leica XML in the .lif |
| LAS X TIF exports | `tif_folder` | Fiji | `MetaData/*.xml` |
| Zeiss `.czi` (file or folder) | `bioformats` | Bio-Formats | Zeiss XML in the .czi (objective, exposure, filters, mode) |
| Nikon `.nd2`, Olympus, OME-TIFF, ... | `bioformats` | Bio-Formats | standard OME fields (vendor reader to add) |

## Steps
| Step | What it does |
|---|---|
| inventory | Lists every image, its acquisition settings, auto-groups comparable images → `manifest/image_manifest.csv` |
| review | Thumbnails + app to confirm experiments, groups, channel names, your fields |
| tune | Interactive B&C per experiment/group → `settings/*.json` |
| render | Full-res max projection + saved B&C → PNGs, composite TIF via Fiji |
| figures | Montages, comparison grids, PowerPoint |

## Running
- **Double-click:** `launch/Start Pipeline (Mac).command` or `(Windows).bat` (see the guides above).
- **From R:** copy `run_session_TEMPLATE.R` (one per session), set `PIPELINE`, `SESSION`, `INPUT`,
  `STEPS`, `INTERACTIVE` at the top, and Source it. A `config.yml` is created next to the data.
- **Set-and-forget:** `INTERACTIVE <- FALSE` skips both apps: saved review/B&C settings are used;
  groups without settings follow `bc:` in config.yml (`auto`, `template` from another session, or `skip`).
- Results: `<name>_pipeline/` (TIF exports: `<name>_tifexport_pipeline/`). Raw files are never touched.
- R packages (installed automatically by the launcher):
  `install.packages(c("yaml", "jsonlite", "xml2", "shiny", "DT", "tiff", "png", "magick", "officer"))`

## When something goes wrong: error reports
If a step fails, the pipeline writes **`<name>_pipeline/logs/error_report_<date-time>.txt`**
and says so in the console (the launcher also shows a message and reveals the file). The
report is plain text and contains:
- the error message, the step, and the chain of function calls that led to it;
- warnings raised during the run;
- pipeline, R, operating-system, R-package and Fiji versions/paths;
- the session's config (without comments), a summary of the manifest (images, groups, flags);
- the last lines of the most recent Fiji logs.

It contains **no image data**, but does include file names and folder paths. If the error
happens before the output folder exists, the report goes next to the config file (or into
`error_reports/` in the pipeline folder).

Who to contact is set in **`support.yml`** (name, email, optional GitHub issues link, and a
short message), shown with every error. To fix a problem, send the report to the maintainer;
when working with Claude on a fix, share the report and the pipeline version it names.

## Fields pre-filled from names
If image or file names contain conditions, animals, slides/coverslips or fields of view,
the inventory pre-fills `experimental_id`, `animal_replicate`, `section_coverslip` and `fov`
(e.g. `(+)BAF_DIV1-...-20X-02-...` -> "(+)BAF DIV1", fov "02"; `SLIDE3-2/Series004` ->
"SLIDE3-2"). Nothing is required; missing information stays blank. Pre-filled values are
yellow in the review app until confirmed or edited. Patterns: `naming:` in config.yml.

## Releasing a new version (maintainer)
1. Bump `VERSION`, add a `CHANGELOG.md` entry, update "Last updated" in `DEVELOPMENT.md`.
2. In R, from this folder: `source("tools/build_release.R")`. It writes
   `dist/scope2fig-<version>.zip`, a ready-to-share copy in which the user guides are plain
   `.txt` (they open in any text editor) and the Mac launcher keeps its run permission.
3. On GitHub: **Releases > Draft a new release**, tag `v<version>`, attach that zip.

`local_settings.yml` (per-computer Fiji path) and `dist/` are never committed (`.gitignore`).

## .lif pixel reader
Bio-Formats 8.1.1 misreads some series in these .lif files (they show the *next* image, shifted,
with a wrapped strip). The pipeline therefore reads .lif pixels itself (`input.lif_reader: 'direct'`):
it finds each image's data block by its ID in the file and uses Leica's byte layout from the XML.
Check it against LAS X exports with `validate_lif_reader(CONFIG, "<export folder>")`.
Previews also warn if two images have identical statistics in every channel.

## Hierarchy
**session** (one .lif) > **experiment** (a staining panel; owns channel names/colors) >
**group** (comparably acquired images; gets its own B&C settings).

## Review app (`steps = "review"`)
Makes thumbnails once (cached in `<name>_pipeline/previews/`), then opens in your browser:
- **Images** — thumbnails, include/exclude, edit experiment, group and your fields
  (double-click a cell, or select rows and "Set for selected rows"). Yellow = guessed from file name.
- **Experiments & channels** — rename experiments; set each channel's label and color once.
- **Groups** — rename/merge groups; warns if a group mixes acquisition settings.
- **Import** — fill fields from your own CSV (matched on `image_uid` or `series_name`).
"Save & close" returns to R. Every save keeps a dated copy in `manifest/history/`.

## B&C app (`steps = "tune"`)
One page per experiment / group: per-channel linear min/max sliders over the real intensity
range, histogram, saturation readout (this image / worst in group), merge + channel panels,
"Show whole group", Auto, copy from another group, or load a settings file from any run.
Saves `<name>_pipeline/settings/<experiment>__<group>.json` + `bc_settings_summary.csv`.

## Render (`steps = "render"`)
Applies each group's saved settings at full resolution. Per image, in `renders/<experiment>/<group>/`:
`_composite.tif` (unaltered data; colors/min/max stored as display settings only),
`_C<n>-<label>.png` per channel, `_merge.png`, optional `_stack.tif`; plus `settings_used.json`.
`renders/render_index.csv` lists every output with your fields and full-res saturation %.
Re-running only redoes images whose settings changed.

## Figures (`steps = "figures"`)
From the rendered PNGs (never the raw data), into `figures/`:
`montages/<experiment>/<group>/<name>_montage.png` (channel grid + larger merge, colored labels,
scale bar from the real pixel size), `grids/<experiment>__<group>_grid.png` (rows = images sorted by
your fields, columns = channels + merge), `<session>_figures.pptx`, `figures_index.csv`.
Layout options are under `figures:` in config.yml.

## What the inventory writes (`<name>_pipeline/manifest/`)
- `image_manifest.csv` — one row per image: size, z-steps/step size, tiles, bit depth,
  pixel size, objective, zoom, laser profile, auto group, flags, and your fields
  (`experimental_id`, `notes`). **Re-running keeps your edits**; old versions go to `history/`.
- `channels.csv` — per image × channel: dye, laser line + %, detector, gain, mode, LAS X LUT.
- `groups_auto.csv` — the comparable groups it found and why.
- `laser_profiles.csv` — distinct laser %/detector/gain settings (L1, L2, ...),
  read from Leica's own XML (in the .lif header, or `MetaData/` for TIF exports).
- `tif_file_index.csv` (TIF mode) — how every file name was interpreted.
- `../logs/` — JSON run log + Fiji console log.

## Tests
`source("tests/test_parse_tif_names.R")` checks the TIF-name parser;
`source("tests/test_leica_xml.R")` then `test_leica_xml("<.lif or MetaData xml>")` checks the Leica XML reader.
