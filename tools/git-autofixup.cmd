@echo off

git commit --fixup=HEAD
set OLD_EDITOR=%EDITOR%
set EDITOR=true
git rebase -i --autostash --autosquash HEAD~2
set EDITOR=%OLD_EDITOR%
