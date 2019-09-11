;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2019  Ascander Dost

;; Author: Ascander Dost
;; Keywords: convenience, tools

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This is my Emacs configuration

;;; Code:

;;; Preliminaries

(setq debug-on-error t)                 ; Enter debugger on error
(setq message-log-max 10000)            ; Keep more log messages

;; Set GC threshold as high as possible for fast startup
(setq gc-cons-threshold most-positive-fixnum)

;; Set GC threshold back to default value when idle
(run-with-idle-timer
 10 nil
 (lambda ()
   (setq gc-cons-threshold (car (get 'gc-cons-threshold 'standard-value)))
   (message "GC threshold restored to %S" gc-cons-threshold)))

;;; Package initialization

(require 'package)
(setq load-prefer-newer t            ; prefer the newest version of a file
      package-enable-at-startup nil) ; explicitly initialize packages

(setq package-archives
      '(("melpa" . "https://melpa.org/packages/")
        ("gnu"   . "http://elpa.gnu.org/packages/")
        ("org"   . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))
(setq-default use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; General

(use-package general
  :demand t
  :config
  
  ;; aliases
  (eval-and-compile
    (defalias 'gsetq #'general-setq)
    (defalias 'gsetq-local #'general-setq-local)
    (defalias 'gsetq-default #'general-setq-default))

  ;; Unbind keys when necessary - General should take precedence
  (general-auto-unbind-keys)
  
  ;; spacemacs like leader key
  (general-create-definer general-spc
    :states '(normal visual insert emacs)
    :keymaps 'override
    :prefix "SPC"
    :non-normal-prefix "C-SPC")

  ;; major mode functionality
  (general-create-definer general-m
    :states 'normal
    :prefix "m"))

;;; Evil and friends

(use-package evil
  :init
  (gsetq evil-want-keybinding nil ; don't load evil bindings for other modes
	 evil-overriding-maps nil ; no maps should override evil maps
	 evil-search-module 'evil-search ; use evil-search instead of isearch
	 evil-ex-search-persistent-highlight nil ; no persistent highlighting after search
	 evil-want-Y-yank-to-eol t)		 ; Y like D
  :config (evil-mode))

(use-package evil-collection
  :after evil
  :config (evil-collection-init))

(use-package evil-escape
  :after evil
  :init (gsetq-default evil-escape-key-sequence "jk")
  :config (evil-escape-mode))

;; Use normal state as the default state for all modes 
(gsetq evil-normal-state-modes
       (append evil-emacs-state-modes
	       evil-normal-state-modes)
       evil-emacs-state-modes nil
       evil-motion-state-modes nil)

;;; OSX settings

(use-package exec-path-from-shell
  :if (eq system-type 'darwin)
  :init (gsetq exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))

(use-package osx-trash
  :if (eq system-type 'darwin)
  :config (osx-trash-setup))

;;; Basic UI

;; Disable tool bar, scroll bar, and menu bar.
;;
;; Note: menu bar cannot be disabled on OSX, so only remove it if
;; we're not on a Mac.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and (not (eq system-type 'darwin)) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

;; Use Emacs' builtin line numbering
(gsetq-default display-line-numbers 'visual ; vim-style line numbers
	       display-line-numbers-widen t ; disregard any narrowing
	       display-line-numbers-current-absolute t) ; display absolute number of current line

(defun ad|relative-line-numbers ()
  (setq-local display-line-numbers 'visual))

(defun ad|absolute-line-numbers ()
  (setq-local display-line-numbers t))

;; Switch to absolute line numbers in insert state
(general-add-hook 'evil-insert-state-entry-hook #'ad|absolute-line-numbers)
(general-add-hook 'evil-insert-state-exit-hook #'ad|relative-line-numbers)

;; Bedazzle the current line number
(custom-set-faces '(line-number-current-line ((t :weight bold
                                                 :foreground "#b58900"))))

;; Set some sensible defaults
(gsetq blink-cursor-mode -1		; no blinking
       ring-bell-function #'ignore	; no ringing
       inhibit-startup-screen t		; no startup screen
       initial-scratch-message nil	; no scratch message
       delete-by-moving-to-trash t)	; delete files to system trash

;; Replace yes/no prompts with y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; No startup message in the echo area
(fset 'display-startup-echo-area-message #'ignore)

;; Display line/column numbers in the mode line
(line-number-mode)
(column-number-mode)

;; Highlight the current line
(global-hl-line-mode)

;;; Fonts and font sizes

(set-face-attribute 'default nil
		    :family "Iosevka Dost"
		    :height 140
		    :weight 'regular)

(set-face-attribute 'variable-pitch nil
		    :family "Fira Sans"
		    :height 140
		    :weight 'regular)

(use-package default-text-scale
  :general
  ("C--" #'default-text-scale-decrease
   "C-=" #'default-text-scale-increase
   "C-0" #'default-text-scale-reset)
  :init
  ;; Unbind default bindings
  (general-def default-text-scale-mode-map
    "C-M--" nil
    "C-M-=" nil
    "C-M-0" nil)
  
  (default-text-scale-mode 1))

;;; Colors and Themes

;; Disable old color theme when switching to new color theme
(defun ad|disable-themes (&rest _)
  "Disable all currently active color themes."
  (mapc #'disable-theme custom-enabled-themes))

(general-add-advice 'load-theme :before #'ad|disable-themes)

(use-package solarized-theme            ; I always come back to you
  :init
  ;; Basic settings - disprefer bold and italics, use high contrast
  (setq solarized-use-variable-pitch nil
        solarized-use-less-bold t
        solarized-use-more-italic nil
        solarized-distinct-doc-face t
        solarized-emphasize-indicators nil
        solarized-high-contrast-mode-line nil)
  ;; Avoid all font size changes
  (setq solarized-height-minus-1 1.0
        solarized-height-plus-1 1.0
        solarized-height-plus-2 1.0
        solarized-height-plus-3 1.0
        solarized-height-plus-4 1.0)
  :config
  ;; Conditionally load the default theme based on whether we're
  ;; running the Emacs daemon.
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'solarized-dark t)))
    (load-theme 'solarized-dark t)))

;;; Coda

;; Display timing information in '*Messages*' buffer
(add-hook 'emacs-startup-hook
	  (lambda ()
	    (message "Emacs ready in %s with %d garbage collections."
		     (format "%.2f seconds"
			     (float-time
			      (time-subtract after-init-time before-init-time))) gcs-done)))

(provide 'init)
;;; init.el ends here
