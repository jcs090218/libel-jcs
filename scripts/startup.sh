#!/bin/sh -e
# ========================================================================
# $File: startup.sh $
# $Date: 2018-11-26 00:56:36 $
# $Revision: $
# $Creator: Jen-Chieh Shen $
# $Notice: See LICENSE.txt for modification and distribution information
#                   Copyright © 2018 by Shen, Jen-Chieh $
# ========================================================================


if [ -n "$TRAVIS" ]; then
    # Make it look like this is ~/.emacs.d (needed for Emacs 26.1, at least)
    export HOME=$PWD/..
    ln -s emacs.d ../.emacs.d
fi

echo "Attempting startup..."

${EMACS:=emacs} -nw --batch \
                --eval '(let ((debug-on-error t)
                              (url-show-status nil)
                              (user-emacs-directory default-directory)
                              (user-init-file (expand-file-name "libel-jcs.el"))
                              (load-path (delq default-directory load-path)))
                           (load-file user-init-file))'

echo "Startup successful"
