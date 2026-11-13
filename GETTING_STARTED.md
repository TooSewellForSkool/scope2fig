# scope2fig: Getting started

scope2fig turns a folder of microscope images into quick-look figures: one montage per image
(each channel + merge, with scale bar), comparison grids, and a PowerPoint. Your original files
are never changed; results go into a new folder next to them.

**Reads:** Leica `.lif`, Zeiss `.czi`, Nikon `.nd2`, and most other formats Fiji can open
(via Bio-Formats), plus TIFs exported from Leica LAS X.

> First time using Terminal or installing software like this? Use the
> **Guide for the uninitiated** (`GUIDE_FOR_THE_UNINITIATED`) instead - same steps, with
> every click spelled out.

## One-time setup (about 15 minutes)

1. **Install R** from https://cran.r-project.org (Mac: pick *arm64* for Apple silicon,
   *x86_64* for Intel). RStudio is not needed.
2. **Install Fiji** from https://fiji.sc. Mac: drag `Fiji.app` into *Applications*.
3. **Unzip scope2fig** into *Documents* and make sure the folder is called `scope2fig`.
   Don't put images inside it.
4. **Mac only - unlock the downloads once.** Open Terminal (Cmd + Space, type *Terminal*,
   Enter), paste this line and press Enter:

   ```
   xattr -dr com.apple.quarantine ~/Documents/scope2fig /Applications/Fiji.app; chmod +x ~/Documents/scope2fig/launch/*.command; echo "All set - you can close this window."
   ```

   **Windows:** if *"Windows protected your PC"* appears on first launch, click
   *More info > Run anyway*.
5. Open Fiji once, run *Help > Update...*, then quit it.

The first run installs a few R packages automatically (needs internet; a few minutes). If Fiji
isn't in a standard place, you'll be asked to locate it once. If macOS asks whether Terminal may
access a folder (Documents, Desktop, an external drive), click **Allow**.

## Each time

1. Double-click **launch > Start Pipeline (Mac).command** (Windows: **Start Pipeline (Windows).bat**).
2. Choose the folder with your images:
   - Leica: the folder containing the `.lif`
   - Zeiss / Nikon: the folder containing the `.czi` / `.nd2` files (one or many per folder)
3. Choose what to do:
   - **Full run (open the review and B&C apps)** - first time for a folder. Two browser pages
     open in turn:
     - *Review*: check which images to use, name your channels, label conditions (values
       guessed from file names are shown in yellow to confirm). Click **Save & close**.
     - *Brightness & contrast*: set each channel's range per group. Click **Save & close**.
   - **Full run without apps** - re-running with settings you already saved (or quick
     automatic ones).
   - **Review images** / **Adjust brightness & contrast** - change just that step.
   - **Re-make renders and figures only** - after changing settings.
4. When it finishes, the results folder opens: `<folder>_pipeline/figures/` has the montages,
   grids and PowerPoint. A Terminal window shows progress meanwhile - leave it open.

## Good to know

- Images are grouped automatically by how they were acquired (objective, image size, laser
  power or exposure time). Brightness/contrast is set per group, so images in a group stay
  directly comparable. Adjustments are linear (min/max) only.
- `renders/.../_composite.tif` files keep the original pixel values (fine for measuring);
  PNGs and figures are for viewing.
- Everything you set is saved (`manifest/`, `settings/`) with a log of what was done.

## If something goes wrong

The pipeline stops, tells you, and saves an **error report**: a text file named
`error_report_<date-time>.txt` in `<folder>_pipeline/logs/` (the launcher opens that folder and
highlights the file). Send it to the contact shown in the message. It holds the technical
details needed to fix the problem, but no images. More fixes for common hiccups are at the end
of the Guide for the uninitiated.
