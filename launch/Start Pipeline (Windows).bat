@echo off
rem Double-click to run the scope2fig.
setlocal
set "PIPE=%~dp0.."
set "RS="
for /f "delims=" %%R in ('dir /b /ad /o-n "%ProgramFiles%\R\R-*" 2^>nul') do (
  if not defined RS set "RS=%ProgramFiles%\R\%%R\bin\Rscript.exe"
)
if not defined RS (
  where Rscript >nul 2>nul && set "RS=Rscript"
)
if not defined RS (
  echo R is not installed. Install it from https://cran.r-project.org and try again.
  pause
  exit /b 1
)
"%RS%" "%PIPE%\launch\launch.R" "%PIPE%"
echo.
pause
