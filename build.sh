#!/bin/sh
emacs -Q --script build-site.el
cd ./RSS
xml_grep ./RSS/ > ./public/rss.xml
