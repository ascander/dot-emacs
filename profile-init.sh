#!/bin/sh

EMACS_CMD=/Applications/Emacs.app/Contents/MacOS/Emacs

$EMACS_CMD -Q -l ./lisp/profile-dotemacs.el -f profile-dotemacs
