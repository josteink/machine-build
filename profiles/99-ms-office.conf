#!/usr/bin/env bash

SETUP=""

echo Obtain MS-office 2010 installer from somewhere.
echo paste in name to SETUP=
echo Remove exit 0 clause from script.
exit 0

# we need these.
sudo apt-get install wine wine-mono0.8 mono-complete ttf-mscorefonts-installer winbind

# make sure we have a clean place to work.
mkdir -p $HOME/Win32
rm -rf $HOME/Win32/Office

#run installer
WINEARCH=win32 WINEPREFIX=~/Win32/Office wine $SETUP

# copy launcher scripts
mkdir -p $HOME/bin/
cp ../data/ms-*.sh $HOME/bin/
