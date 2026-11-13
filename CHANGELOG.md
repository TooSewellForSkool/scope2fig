# Changelog

All dates 2026. Author: Jonathon Sewell, developed with Claude (Anthropic).

## 1.4.0 - 2026-11-13
- **Guide for the uninitiated** (`GUIDE_FOR_THE_UNINITIATED.md`): step-by-step setup for people
  new to Terminal and R - installing R and Fiji, one copy-paste Terminal line that unblocks the
  downloads (scope2fig and Fiji) and makes the launcher runnable, folder-access pop-ups, Windows
  SmartScreen, and fixes for common problems.
- README rewritten as a landing page (what you get, "Start here" by experience level).
- User guides are Markdown in the repository; `tools/build_release.R` builds the release zip with
  plain-text copies (`.txt`) and the Mac launcher kept executable.
- GitHub bug-report template asking for the error report; GitHub links point to github.com/TooSewellForSkool/scope2fig; `support.yml` shows the issues link with every error report.

## 1.3.0 - 2026-11-06
- Renamed from "confocal_pipeline" to **scope2fig** (it reads any microscope format).
  Folder, file headers, docs, launcher and error-report titles updated. No change in behaviour;
  existing sessions and their outputs work as before. R users: update `PIPELINE` in run scripts.

## 1.2.0 - 2026-11-06
- **Fields pre-filled from names** when the information is there (never required): condition
  ("(+)BAF", "+BAF", WT/KO/cKO/Ctrl/Vehicle..., combined with DIV/DPI timepoints, e.g.
  "(+)BAF DIV1") -> experimental_id; mouse3/rep2 -> animal_replicate; SLIDE3-2/coverslip1/well4
  -> section_coverslip; FoV2/pos3 and Zen's "20X-02-" -> fov. Shown in yellow to confirm.
  Configurable per field (`naming.name_patterns`, `naming.parse_names`).
- Re-running the inventory keeps values you typed or imported, and refreshes values that were
  only pre-filled (so older sessions get the new pre-fill).
- `tests/test_name_patterns.R`.
- `GETTING_STARTED` is now a plain-text file (`GETTING_STARTED.txt`) so it opens in any text editor.

## 1.1.0 - 2026-11-06
- **Error reports:** if a step fails, `logs/error_report_<time>.txt` is written (error, where it
  happened, warnings, versions, config, manifest summary, end of Fiji logs; no image data). The
  console and the launcher say where it is and whom to send it to (`support.yml`).
- Documentation: `DEVELOPMENT.md` (authorship, history, design), this changelog, author/version
  header in every source file.

## 1.0.0 - 2026-11-06
- Any Bio-Formats format (`bioformats` mode): Zeiss `.czi` (with Zeiss metadata: objective,
  exposure, filters, acquisition mode), Nikon `.nd2` and others (standard OME metadata).
- Input auto-detected from a folder (.lif, vendor files, or LAS X TIF exports).
- Double-click launchers for Mac/Windows with folder picker, menu, package install, Fiji lookup.
- `GETTING_STARTED.txt` for non-coders; personal paths removed from templates.

## 0.9 - 2026-11-06
- Built-in .lif pixel reader (fixes Bio-Formats misreading 5 series) and `validate_lif_reader()`.
- Duplicate-data check; previews/renders regenerate when the pixel reader changes.
- Run-script template, `input` override, `steps = "all"`, unattended mode with `bc.fallback`
  (auto / template / skip). TIF-export runs get their own output folder.

## 0.5 - 2026-11-05/06
- Module 5: montages, comparison grids, PowerPoint (30 pt titles, B&C values in footers).
- Module 4: full-resolution render (composite TIF with unaltered data, gray channel PNGs, merge).
- Module 3: B&C app (sliders + typed values, histograms, saturation readout, group view,
  copy/load settings, settings history).
- Module 2: review app (thumbnails, experiments, groups, channel names/colors, user fields, import).
- Module 1: inventory from .lif via Bio-Formats + Leica XML (lasers, detectors, zoom, tiles),
  auto-grouping, laser-profile tolerance, tile handling, TIF-export fallback.
