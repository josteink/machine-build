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

echo - gnutls
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/gnutls-3.3.11-w32-bin.zip"

echo - libpng
curl -O "http://skylink.dl.sourceforge.net/project/ezwinports/libpng-1.6.12-w32-bin.zip"

echo - libsvg
curl -O "http://vorboss.dl.sourceforge.net/project/ezwinports/librsvg-2.40.1-2-w32-bin.zip"

echo - libxml
curl -O "http://kent.dl.sourceforge.net/project/ezwinports/libxml2-2.7.8-w32-bin.zip"

echo - zlib
curl -O "http://netassist.dl.sourceforge.net/project/ezwinports/zlib-1.2.8-2-w32-bin.zip"

echo Unpacking...
cd ..

unzip download/emacs-24.5-bin-i686-mingw32.zip
unzip -o download/gnutls-3.3.11-w32-bin.zip
unzip -o download/libpng-1.6.12-w32-bin.zip
unzip -o download/librsvg-2.40.1-2-w32-bin.zip
unzip -o download/libxml2-2.7.8-w32-bin.zip
unzip -o download/zlib-1.2.8-2-w32-bin.zip

echo Emacs ready and unpacked at %TMP%\Emacs
pause >NIL
