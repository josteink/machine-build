#!/usr/bin/env bash

# script-folder, without trailing /
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $ROOT/include.sh

function run-all()
{
    # neatly excludes ourselves by using setup- wildcard.
    TMP=/tmp/setup$$.tmp
    find $ROOT/profiles -type f -name '*.conf' -printf "%p $*\n" | sort >$TMP
    source $TMP
    rm $TMP
}

case "$1" in
    machine|user|report)
        run-all $*
        ;;
    *)
        echo "Specify setup-type: machine or user? Specify optional profile (desktop/server/development)"
        exit 1
esac
