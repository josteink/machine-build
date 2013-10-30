#!/usr/bin/env bash

INSTALL="sudo apt-get install -y"

function validate_can_elevate()
{
    if [ "`sudo whoami`" != "root" ] ; then
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
    # interactive and not-interactive?
    # still need machine and not user?
    # config "level"? machine level. user level?
    # ./setup.sh -all | -machine | -user
    # -all = machine interactive > machine > user interactive > user
    # two distinct interactive phases, but user interactive can reasonably depend on "machine".
    # make it more conventiony than frameworky. avoid second rewrite.
    # machine "things" run as user, but CAN elevate as needed.

    validate_user
    validate_can_elevate

    if [ "$2" == "" ] ; then
        machine $*
    else
        if [ "$2" == "$PROFILE" ] ; then
            machine $*
        fi
    fi

}

function do_user()
{
    # TODO: remember to check for correct profile.

    validate_user

    if [ "$2" == "" ] ; then
        user $*
    else
        if [ "$2" == "$PROFILE" ] ; then
            user $*
        fi
    fi
}

function do_report()
{
    if [ "$2" == "" ] ; then
        if [ "$PROFILE" == "" ] ; then
            echo "$NAME"
        else
            echo "$NAME [$PROFILE]"
        fi
    else
        if [ "$PROFILE" == "$2" ] ; then
            echo "$NAME"
        fi
    fi
}

function process()
{
    case "$1" in
        machine)
            do_machine $*
            ;;
        user)
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
