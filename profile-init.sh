#!/bin/sh

EMACS_CMD=${HOME}/.nix-profile/bin/emacs

$EMACS_CMD -Q -l ./lisp/profile-dotemacs.el -f profile-dotemacs
