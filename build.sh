#!/bin/sh
rm -r public
emacs -Q --script build-site.el
