@echo off

echo Synching machine settings from git origin.

echo Script drive:
echo %~d0
%~d0

echo Script dir:
echo %~dp0
cd %~dp0

echo Git root:
cd ..
cd

echo Remote:
git remote -v

echo Synching settings...
git pull --rebase


echo Done.
pause >nul