@echo off

echo Creating workspace
cd %TMP%
mkdir emacs
cd emacs

echo Cleaning workspace
REM del *.* /s /q

echo Creating download-folder
mkdir download
cd download

echo Getting Emacs main...
REM curl -L -O "http://ftp.gnu.org/gnu/emacs/windows/emacs-25.1-x86_64-w64-mingw32.zip"

echo Getting msys2 (64-bit) (source http://msys2.github.io/)...
REM curl -L -O http://repo.msys2.org/distrib/x86_64/msys2-x86_64-20160205.exe

echo Install msys2 using defaults options and execute the following commands:
echo.
echo pacman -Sy pacman
echo pacman -S  mingw-w64-x86_64-xpm-nox mingw-w64-x86_64-libtiff mingw-w64-x86_64-giflib mingw-w64-x86_64-libpng mingw-w64-x86_64-libjpeg-turbo mingw-w64-x86_64-librsvg mingw-w64-x86_64-libxml2 mingw-w64-x86_64-gnutls mingw-w64-x86_64-zlib
echo.
echo Once done, continue by pressing any key
pause >nul

echo
echo Unpacking!

cd.. 
unzip download/emacs-25.1-x86_64-w64-mingw32.zip
cd bin
xcopy C:\msys64\mingw64\bin\*.dll . /y

echo Emacs ready and unpacked at %TMP%\Emacs
pause >NIL
