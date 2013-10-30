#!/usr/bin/env bash

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
    # TODO: remember to check for correct profile.

    # validate_not_root
    # validate_can_elevate # `sudo whoami` = root
    # interactive and not-interactive?
    # still need machine and not user?
    # config "level"? machine level. user level?
    # ./setup.sh -all | -machine | -user
    # -all = machine interactive > machine > user interactive > user
    # two distinct interactive phases, but user interactive can reasonably depend on "machine".
    # make it more conventiony than frameworky. avoid second rewrite.
    # machine "things" run as user, but CAN elevate as needed.

    validate_root
    machine $*
}

function do_user()
{
    # TODO: remember to check for correct profile.

    validate_user
    user $*
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
            do_machine $*
            ;;
        user)
            validate_user
            do_user $*
            ;;
        report)
            do_report $*
            ;;
        *)
            echo "Unknown request \"$1\"."
            exit 1
            ;;
    esac
}
