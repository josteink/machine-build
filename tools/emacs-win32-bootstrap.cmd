@echo off

echo Creating workspace
cd %TMP%
mkdir emacs
cd emacs

echo Cleaning workspace
del *.* /s /q

echo Creating download-folder
mkdir download
cd download

echo Getting Emacs main...
curl -O "http://ftp.gnu.org/gnu/emacs/windows/emacs-24.5-bin-i686-mingw32.zip"

echo Getting ezwinports (source http://sourceforge.net/projects/ezwinports/files/)...

curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/gnutls-3.3.11-w32-bin.zip"
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/giflib-5.1.0-w32-bin.zip"
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/jpeg-v9a-w32-bin.zip"
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/libpng-1.6.12-w32-bin.zip"
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/librsvg-2.40.1-2-w32-bin.zip"
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/libxml2-2.7.8-w32-bin.zip"
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/zlib-1.2.8-2-w32-bin.zip"

echo Unpacking...
cd ..

unzip download/emacs-24.5-bin-i686-mingw32.zip
unzip -o download/gnutls-3.3.11-w32-bin.zip
unzip -o download/giflib-5.1.0-w32-bin.zip
unzip -o download/jpeg-v9a-w32-bin.zip
unzip -o download/libpng-1.6.12-w32-bin.zip
unzip -o download/librsvg-2.40.1-2-w32-bin.zip
unzip -o download/libxml2-2.7.8-w32-bin.zip
unzip -o download/zlib-1.2.8-2-w32-bin.zip

echo Emacs ready and unpacked at %TMP%\Emacs
pause >NIL
