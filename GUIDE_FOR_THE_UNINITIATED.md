# scope2fig: Guide for the uninitiated

Never opened Terminal? Not sure what "R" is? Perfect - this guide is for you. Follow it once,
top to bottom, and you'll never need it again. Nothing here can break your computer or your
images: scope2fig only *reads* your files and writes its results into a new folder.

**What you need:** a Mac or Windows computer, internet, your computer password, ~20 minutes.

**Jump to:** [Mac](#mac) | [Windows](#windows) | [When something looks wrong](#when-something-looks-wrong)

---

## Mac

### Step 1 - Install R (the engine)

1. Find out which kind of Mac you have: click the Apple menu (top-left corner of the screen)
   > **About This Mac**. Look at **Chip** (or **Processor**):
   - says **Apple M1/M2/M3/M4...** - you have an *Apple silicon* Mac
   - says **Intel** - you have an *Intel* Mac
2. Go to https://cran.r-project.org > **Download R for macOS**.
3. Download the `.pkg` file that matches your Mac (*arm64* = Apple silicon, *x86_64* = Intel).
4. Open the downloaded file and click **Continue / Agree / Install** until it says it's done.
   (You never need to open R itself.)

### Step 2 - Install Fiji (the image reader)

1. Go to https://fiji.sc and download Fiji for macOS (pick the Apple silicon or Intel version,
   same as above).
2. Open your **Downloads** folder. If the download is a `.zip`, double-click it to unzip.
3. Drag **Fiji.app** into your **Applications** folder.
4. Don't open it yet - Step 4 unlocks it first.

### Step 3 - Get scope2fig

1. On the scope2fig GitHub page, either download the latest file under **Releases** (right side
   of the page), or click the green **Code** button > **Download ZIP**.
2. In **Downloads**, double-click the zip to unzip it (Safari may already have done this).
3. If the folder is called `scope2fig-main` (or `scope2fig-1.4.0`), rename it to just
   **`scope2fig`** (click the name once, wait, click again, type).
4. Drag the `scope2fig` folder into your **Documents** folder. Don't put images inside it.

### Step 4 - Unlock everything (one copy-paste in Terminal)

macOS is suspicious of anything downloaded from the internet and blocks it with a scary
message. One command tells your Mac "I trust these two downloads".

1. **Open Terminal:** press **Cmd + Space** (opens Spotlight search), type **Terminal**,
   press **Enter**. A window with text appears. Congratulations - you're now tapped into the
   Matrix. (It's just a place where you type instructions instead of clicking.)
2. **Copy** this whole line (it's one line - triple-click it to select it all):

   ```
   xattr -dr com.apple.quarantine ~/Documents/scope2fig /Applications/Fiji.app; chmod +x ~/Documents/scope2fig/launch/*.command; echo "All set - you can close this window."
   ```

3. Click in the Terminal window, **paste** (Cmd + V) and press **Enter**.
4. You should see "All set - you can close this window." That's it! Close Terminal.
   (If a line *above* it says "No such file or directory", see just below.)

> What did that do? `xattr ...` removes the "downloaded from the internet" tag from the
> scope2fig folder and from Fiji; `chmod ...` marks the launcher as something that may run.
> Nothing is deleted or installed.

**If it says "No such file or directory":** the folder isn't at `Documents/scope2fig` or Fiji
isn't in Applications. Fix the name/location (Step 2.3 or 3.3-3.4) and paste the line again.
Alternatively type `xattr -dr com.apple.quarantine ` (with a space at the end), drag the
scope2fig folder from Finder onto the Terminal window, and press Enter.

### Step 5 - First run

1. Open Fiji once (Applications > Fiji), click **Help > Update...**, let it finish, quit Fiji.
2. Open `Documents > scope2fig > launch` and double-click **Start Pipeline (Mac).command**.
3. A Terminal window opens - that's normal, it shows progress. **Leave it open.**
4. The first time, it installs some R add-ons. Wait a few minutes until something happens.
5. **Permission pop-ups:** macOS may ask *"Terminal would like to access files in your
   Documents folder"* (or Desktop, Downloads, or a *removable volume* for external drives).
   Click **Allow** - scope2fig needs to read your images and write results next to them.
6. A window asks you to **choose the folder with your images**:
   - Leica: the folder that contains the `.lif` file
   - Zeiss / Nikon / others: the folder that contains the `.czi` / `.nd2` files
7. Pick what to do. First time: **Full run (open the review and B&C apps)**. Your web browser
   opens the apps; when you're done in each, click **Save & close**.
8. When it finishes, the results folder opens on its own. Your figures are in
   `figures/` (montages, comparison grids and a PowerPoint).

From now on, just do Step 5.2 onward.

---

## Windows

### Step 1 - Install R
1. Go to https://cran.r-project.org > **Download R for Windows** > **base** >
   **Download R-x.x.x for Windows**.
2. Run the installer and click **Next** until it finishes (the defaults are fine).

### Step 2 - Install Fiji
1. Go to https://fiji.sc and download Fiji for Windows.
2. Right-click the zip > **Extract All...** and extract it to `C:\` or your Documents folder
   (you'll get a folder called **Fiji.app**).
3. Open Fiji once (double-click the Fiji / ImageJ program inside Fiji.app), click
   **Help > Update...**, let it finish, close Fiji.

### Step 3 - Get scope2fig
1. On the GitHub page, download the latest **Release**, or **Code > Download ZIP**.
2. Right-click the zip > **Extract All...** > choose your **Documents** folder.
   (Important: don't run it from inside the zip - extract it first.)
3. If the folder is called `scope2fig-main`, rename it to **scope2fig**.

### Step 4 - First run
1. Open `Documents > scope2fig > launch` and double-click **Start Pipeline (Windows).bat**.
2. If Windows says **"Windows protected your PC"**: click **More info** > **Run anyway**.
   (This only appears the first time.)
3. A black window opens and shows progress - leave it open. The first time, it installs some
   R add-ons (a few minutes).
4. Choose the folder with your images, then type the number of what you want to do and press
   **Enter**. First time: **1** (full run with the apps).
5. When it finishes, the results folder opens. Figures are in `figures/`.

---

## When something looks wrong

- **"R is not installed"** - redo Step 1, then double-click the launcher again.
- **It asks you to locate Fiji** - point it to **Fiji.app** (Mac: in Applications; Windows:
  where you extracted it). It remembers from then on.
- **Mac: "...cannot be opened because it is from an unidentified developer"** or
  **"...was blocked"** - Step 4 didn't apply. Paste the line from Step 4 again. Last resort:
  *System Settings > Privacy & Security*, scroll down to the blocked item > **Open Anyway**.
- **Mac: "...you don't have appropriate access privileges"** - paste the line from Step 4 again.
- **Mac: "Operation not permitted" or images on an external drive can't be read** - open
  *System Settings > Privacy & Security > Files and Folders* > **Terminal** and switch on the
  folders you use (Documents, Desktop, Downloads, Removable Volumes). If your images live
  somewhere unusual, *Full Disk Access* > add **Terminal** also works.
- **Double-clicking does nothing / the window closes immediately** - open the launcher again
  and read the last lines in the window before it closes; if it mentions an error report,
  see below.
- **The browser page looks stuck** - it's fine to close the tab; the pipeline continues with
  what you saved. Check the Terminal/black window for messages.

**Still stuck?** When scope2fig hits a problem it saves an **error report** (a text file
named `error_report_<date-time>.txt`, in `<your folder>_pipeline/logs/`) and shows you
where it is. Send that file to the contact shown in the message. It contains the technical
details needed to help - but none of your images.
