;;; minimal-init.el -- My minimal configuration of Emacs -*- lexical-binding: t -*-

;; Author: berquerant
;; Maintainer: berquerant
;; Created: 14 Jan 2023
;; Version: 0.1.7
;; Keywords: init
;; URL: https://github.com/berquerant/emacs-minimal-init-el

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 3, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; - Change settings of built-in packages
;; - Does not change key bindings explicitly
;; - Does not install 3rd party packages

;;; Code:

(defgroup minimal-init nil
  "Minimal configuration of Emacs."
  :prefix "minimal-init-"
  :group 'minimal-init)

(defcustom minimal-init-language-name "Japanese"
  "Language name to be set.")

(defcustom minimal-init-coding-system 'utf-8
  "Coding system to be set.")

(defcustom minimal-init-default-major-mode 'text-mode
  "Default major mode.")

(defcustom minimal-init-gc-threshold (* 100 (expt 2 20)) ; 100 MiB
  "Number of bytes to start gc.")

(defcustom minimal-init-display-time t
  "If t then `display-time'.")

(defcustom minimal-init-default-directory "~/"
  "Name of default directory of current buffer.")

(defun minimal-init--default-directory ()
  (expand-file-name minimal-init-default-directory))

(defun minimal-init--setup-display-time ()
  (when minimal-init-display-time
    (setq display-time-interval 1 ; display time every second
          display-time-string-forms '((format-time-string "%Y-%m-%d %H:%M:%S" now)))
    (display-time) ; display time on modeline
    (run-with-idle-timer 60 t #'display-time)))

(defun minimal-init--setup-interprogram-darwin-paste ()
  (shell-command-to-string "pbpaste"))

(defun minimal-init--setup-interprogram-darwin-cut (text &optional push)
  (let ((process-connection-type nil))
    (let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
      (process-send-string proc text)
      (process-send-eof proc))))

(defun minimal-init--setup-interprogram-cut-and-paste-darwin ()
  (setq interprogram-cut-function 'minimal-init--setup-interprogram-darwin-cut
        interprogram-paste-function 'minimal-init--setup-interprogram-darwin-paste))

(defun minimal-init--setup-interprogram-cut-and-paste ()
  (setq save-interprogram-paste-before-kill t)
  (cond ((string= system-type "darwin") (minimal-init--setup-interprogram-cut-and-paste-darwin))
        ((string= system-type "gnu/linux") nil)
        (t (display-warning 'minimal-init
                            (format "Failed to setup interprogam cut-and-paste because OS %s not supported"
                                    system-type)))))

(defun minimal-init--setup()
  ;;; language
  (set-language-environment minimal-init-language-name)
  (when (eq minimal-init-language-name "Japanese") ; ignore spell check for Japanese
    (add-to-list 'ispell-skip-region-alist '("[^\000-\377]+")))
  (setq-default bidi-display-reordering nil) ; ignore the writing direction: right to left

  ;;; confing system
  (dolist (f '(set-buffer-file-coding-system
               prefer-coding-system
               set-terminal-coding-system
               set-default-coding-systems))
    (funcall f minimal-init-coding-system))
  (setq file-name-coding-system minimal-init-coding-system
        locale-coding-system minimal-init-coding-system)

  ;;; font
  (set-face-attribute 'default nil :height 100) ; initial font size

  ;;; modeline
  (setcar mode-line-position ; display the number of lines on mode line
          '(:eval (format "%d" (count-lines (point-max) (point-min)))))

  ;;; frame
  (setq frame-title-format (format "%%f - Emacs@%s" (system-name)))

  ;;; default mode
  (setq-default major-mode minimal-init-default-major-mode) ; instead of fundamental-mode
  (setq initial-major-mode minimal-init-default-major-mode)

  ;;; bars
  (menu-bar-mode -1) ; hide menu bar
  (tool-bar-mode -1) ; hide tool bar
  (toggle-scroll-bar -1) ; hide scroll bars
  (toggle-horizontal-scroll-bar -1) ; hide horizontal scroll bars

  ;;; linum
  (global-display-line-numbers-mode 1) ; show line nunbers
  (column-number-mode 1) ; show column number
  ;; no realistic limits to display linum
  (setq line-number-display-limit 100000
        line-number-display-limit-width 50)

  ;;; eval
  (setq eval-expression-print-length nil ; no limit for printing evaluated value
        eval-expression-print-level nil
        eval-expression-print-maximum-character nil
        max-lisp-eval-depth 1600 ; recursion limit
        read-process-output-max (* 1024 1024))

  ;;; garbage collection
  (setq gc-cons-threshold minimal-init-gc-threshold ; reduce gc frequency
        gc-cons-percentage 1
        garbage-collection-messages nil) ; hide gc message

  ;;; history
  (setq history-delete-duplicates t
        history-length 1000 ; minibuffer history
        message-log-max 10000
        completion-ignore-case t
        kill-ring-max 200) ; copy history
  (savehist-mode 1) ; save minibuffer history
  (winner-mode 1) ; save window configurations

  ;;; yes or no query
  (fset 'yes-or-no-p 'y-or-n-p) ; convert yes/no query into y/n
  (setq confirm-kill-emacs 'y-or-n-p)

  ;;; compile
  (setq compilation-scroll-output t ; tail when compile
        package-native-compile t) ; version:28.1

  ;;; tab
  (setq-default tab-width 2
                indent-tabs-mode nil) ; disable tab

  ;;; newline
  (setq require-final-newline nil
        next-line-add-newlines nil)

  ;;; directory, links
  (setq vc-follow-symlinks t
        default-directory (minimal-init--default-directory))

  ;;; disable files
  (setq make-backup-files nil ; no *.~ files
        auto-save-default nil ; no .#* files
        create-lockfiles nil) ; no lock files

  ;;; initial screen
  (setq inhibit-splash-screen t ; no splash screen
        inhibit-startup-message t ; no startup
        initial-scratch-message nil) ; empty scratch

  ;;; revert
  (setq revert-without-query '(".*"))
  (global-auto-revert-mode 1) ; revert buffer when the file changes

  ;;; etc
  (show-paren-mode 1) ; highlight matching parens
  (delete-selection-mode 1) ; overwrite text if region is slelected
  (setq ring-bell-function 'ignore  ; no beep
        yank-excluded-properties t    ; ignore text properties when paste
        set-mark-command-repeat-pop t ; pop-global-mark like smartrep
        x-alt-keysym 'meta            ; for X Windows c.f. https://www.emacswiki.org/emacs/MetaKeyProblems#h5o-9
        split-height-threshold 120)
  (minimal-init--setup-interprogram-cut-and-paste)
  (minimal-init--setup-display-time))

;;;###autoload
(defun minimal-init-setup ()
  "Setup minimal emacs settings."
  (minimal-init--setup))

(provide 'minimal-init)
;;; minimal-init.el ends here.
