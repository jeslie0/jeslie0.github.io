#!/bin/sh
emacs -Q --script build-site.el

# if [[ -d ./public/images/ ]]; then
#     cp -r ./content/images/* ./public/images/
# else
#     mkdir ./public/images/ && cp -r ./content/images/* ./public/images
# fi

# if [[ -d ./public/CSS/ ]]; then
#     cp -r ./content/CSS/* ./public/CSS
# else
#     mkdir ./public/CSS && cp -r ./content/CSS/* ./public/CSS
# fi
