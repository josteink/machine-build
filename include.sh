#!/usr/bin/env bash

# script-folder, without trailing /
SCRIPT_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

VALIDATE="$SCRIPT_DIR/validate"

function validate_root()
{
}

function validate_user()
{
    # user = no-root

}

function process()
{
    case "$1" in
        machine)
            validate_root
            machine
            ;;
        user)
            validate_user
            user
            ;;
        report)
            report
            ;;
    esac
}
