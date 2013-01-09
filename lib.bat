@echo off
set VISUAL_STUDIO=C:\Program Files (x86)\Microsoft Visual Studio 10.0
set SETUP_ENV="%VISUAL_STUDIO%\VC\bin\vcvars32.bat"

call %SETUP_ENV%
lib.exe %*
