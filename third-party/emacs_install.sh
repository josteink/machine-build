#!/usr/bin/env bash

source ../includes.sh

$PREVALIDATE_ROOT

add-apt-repository ppa:cassou/emacs
apt-get update
apt-get install -y emacs24 emacs24-el emacs24-common-non-dfsg
