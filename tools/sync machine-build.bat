@echo off
set GITROOT=/cygdrive/d/Git/machine-build

REM two remarks about this invocation:
REM
REM 1. we need to be explicit about WHICH bash we're launching, or we might
REM    launch msysgit's bash. This does not do what we want.
REM
REM 2. we need to invoke our scripts through a interactive logged in bash
REM    otherwise we will not be in "cygwin"-mode and git will have issues with
REM    file-permissions and think we're in a dirty state.
REM
REM With that out of the way, pass on control to the script which does the
REM actual work.
C:\cygwin64\bin\bash.exe --login -i "%GITROOT%/tools/sync machine-build.sh"