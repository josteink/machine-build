#!/usr/bin/env bash

# script-folder, without trailing /
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

INSTALL="apt-get install -y"

function validate_root()
{
    if [ "`whoami`" != "root" ] ; then
        echo "User is NOT root. Please elevate."
        exit 1 ;
    fi
}

function validate_user()
{
    if [ "`whoami`" == "root" ] ; then
        echo "User IS root. Please run command unelevated."
        exit 1 ;
    fi
}

function do_machine()
{
    # remember to check for correct profile.

    validate_root
    machine
}

function do_user()
{
    # remember to check for correct profile.

    validate_user
    user
}

function do_report()
{
    if [ "$PROFILE" == "" ] ; then
        echo "$NAME"
    else
        echo "$NAME [$PROFILE]"
    fi
}

function process()
{
    case "$1" in
        machine)
            validate_root
            do_machine
            ;;
        user)
            validate_user
            do_user
            ;;
        report)
            do_report
            ;;
    esac
}
