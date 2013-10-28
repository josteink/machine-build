#!/usr/bin/env bash

# script-folder, without trailing /
ROOT="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

INSTALL="apt-get install -y"

function validate_root()
{
    # TODO: check for root.
}

function validate_user()
{
    # user = no-root
    # TODO: check for user.
}

function do_machine()
{
    # TODO: add support for pre-requisites in one pre-run step?

    # remember to check for correct profile.
    # TODO: call machine-function
}

function do_user()
{
    # remember to check for correct profile.

    # TODO: call machine-function (but tidy up profiles first)
    # also: create a "base-template" to copy up for new apps.
}

function do_report()
{
    # remember to check for profile.
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
