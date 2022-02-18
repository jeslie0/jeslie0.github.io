#!/bin/sh
emacs -Q --script build-site.el

cp ./RSS/rss.xml ./public/
