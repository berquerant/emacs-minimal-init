#!/bin/bash

set -ex

git clone "https://github.com/berquerant/emacs-minimal-init.git" "${EMACSD}/site-lisp/emacs-minimal-init"
cat - <<EOS >> "${EMACSD}/init.el"
(add-to-list 'load-path "${EMACSD}/site-lisp/emacs-minimal-init")
(require 'minimal-init)
(minimal-init-setup)
EOS
