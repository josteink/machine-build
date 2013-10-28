#!/usr/bin/env bash

# script-folder, without trailing /
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
source $SCRIPT_DIR/include.sh

function run-all()
{
    # neatly excludes ourselves by using setup- wildcard.
    TMP=/tmp/setup$$.tmp
    find $SCRIPT_DIR/profiles -type f -name '*.conf' -printf '%p $*\n' | sort >$TMP
    source $TMP
    rm $TMP
}

case "$1" in
    machine)
        $VALIDATE/root.sh
        run-all
        ;;
    user)
        $VALIDATE/non-root.sh
        run-all
        ;;
    report)
        run-all
        ;;
    *)
        echo "Specify setup-type: machine or user? Specify optional profile (desktop/server/development)"
        exit 1
esac