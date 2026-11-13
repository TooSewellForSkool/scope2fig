#!/bin/bash
# Double-click to run the scope2fig.
# First time only: right-click this file > Open > Open (macOS blocks downloaded scripts once).
PIPE="$(cd "$(dirname "$0")/.." && pwd)"
RS=""
# Prefer the standard R install from CRAN over conda/other copies of R
for c in /Library/Frameworks/R.framework/Resources/bin/Rscript /usr/local/bin/Rscript \
         /opt/homebrew/bin/Rscript "$(command -v Rscript)"; do
  if [ -n "$c" ] && [ -x "$c" ]; then RS="$c"; break; fi
done
if [ -z "$RS" ]; then
  osascript -e 'display alert "R is not installed" message "Install R from https://cran.r-project.org, then double-click this file again."'
  exit 1
fi
"$RS" "$PIPE/launch/launch.R" "$PIPE"
echo
read -n 1 -s -r -p "Done. Press any key to close this window."
