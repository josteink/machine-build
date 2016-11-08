@echo off

set URL=%*
if "%URL%" == "" %0 http://ftp.gnu.org/gnu/emacs/windows/emacs-24.5-bin-i686-mingw32.zip

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
%WGET% -c "%URL%"

echo Getting ezwinports (source http://sourceforge.net/projects/ezwinports/files/)...

%WGET% -c "http://netix.dl.sourceforge.net/project/ezwinports/gnutls-3.3.11-w32-bin.zip"
%WGET% -c "http://netix.dl.sourceforge.net/project/ezwinports/giflib-5.1.0-w32-bin.zip"
%WGET% -c "http://netix.dl.sourceforge.net/project/ezwinports/jpeg-v9a-w32-bin.zip"
%WGET% -c "http://netix.dl.sourceforge.net/project/ezwinports/libpng-1.6.12-w32-bin.zip"
%WGET% -c "http://netix.dl.sourceforge.net/project/ezwinports/librsvg-2.40.1-2-w32-bin.zip"
%WGET% -c "http://netix.dl.sourceforge.net/project/ezwinports/libxml2-2.7.8-w32-bin.zip"
%WGET% -c "http://netix.dl.sourceforge.net/project/ezwinports/zlib-1.2.8-2-w32-bin.zip"

cd ..

echo Cleaning workspace
del unpacked /s /q
mkdir unpacked

echo Unpacking...
cd unpacked

unzip ../download/%EMACS_ZIP%
unzip -o ../download/gnutls-3.3.11-w32-bin.zip
unzip -o ../download/giflib-5.1.0-w32-bin.zip
unzip -o ../download/jpeg-v9a-w32-bin.zip
unzip -o ../download/libpng-1.6.12-w32-bin.zip
unzip -o ../download/librsvg-2.40.1-2-w32-bin.zip
unzip -o ../download/libxml2-2.7.8-w32-bin.zip
unzip -o ../download/zlib-1.2.8-2-w32-bin.zip

echo Emacs ready and unpacked at %TMP%\Emacs\Unpacked
pause >NUL