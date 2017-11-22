@echo off

set URL=%1
if "%URL%"=="" %0 "http://ftp.gnu.org/gnu/emacs/windows/emacs-25.3_1-x86_64.zip"

set WGET=%~dp0%wget.exe
echo Using wget: %WGET%

echo Creating workspace
REM change drive first. TMP may be on different drive!
%TMP:~0,2%
cd %TMP%
mkdir emacs
cd emacs

echo Creating download-folder
mkdir download
cd download

set EMACS_ZIP=%~nx1
echo Getting Emacs main (%EMACS_ZIP%)...
echo Downloading from: %URL%
%WGET% -c "%URL%" >NUL 2>NUL

echo Getting extra DLLs...

%WGET% -c "http://ftp.gnu.org/gnu/emacs/windows/emacs-25-x86_64-deps.zip" >NUL 2>NUL

cd ..

echo Cleaning workspace...
del unpacked /s /q >NUL 2>NUL
mkdir unpacked
echo Done

echo Unpacking...
cd unpacked

"c:\Program Files\7-Zip\7z.exe" x -y ../download/%EMACS_ZIP%
"c:\Program Files\7-Zip\7z.exe" x -y ../download/emacs-25-x86_64-deps.zip

echo Emacs ready and unpacked at %TMP%\Emacs\Unpacked
pause >NIL
