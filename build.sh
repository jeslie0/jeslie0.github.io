#!/bin/sh
emacs -Q --script build-site.el
xml_grep ./RSS/ > ./public/rss.xml
