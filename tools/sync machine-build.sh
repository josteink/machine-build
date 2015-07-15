#!/bin/sh

# get script-folder, without trailing /
CURRENT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
ROOT="$CURRENT/.."

cd $ROOT
# rebase to avoid getting merge-conflicts if we can avoid it!
git pull --rebase
