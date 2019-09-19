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

;; This is my Emacs config. There are many like it, but this one is mine.

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

;;; General.el

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

  ;; General leader key
  (general-create-definer general-spc
    :states 'normal
    :keymaps 'override
    :prefix "SPC")

  ;; Window navigation/management, Version control, etc.
  (general-create-definer general-t
    :states 'normal
    :keymaps 'override
    :prefix "t")

  ;; Major mode functionality
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
       (append evil-emacs-state-modes evil-normal-state-modes)
       evil-emacs-state-modes nil
       evil-motion-state-modes nil)

;;; OSX settings

(defconst ad:is-a-mac-p (eq system-type 'darwin) "Are we on a Mac?")

(use-package exec-path-from-shell
  :if ad:is-a-mac-p
  :init (gsetq exec-path-from-shell-check-startup-files nil)
  :config (exec-path-from-shell-initialize))

(use-package osx-trash
  :if ad:is-a-mac-p
  :config (osx-trash-setup))

;; Modifier keys
(setq mac-command-modifier 'meta	; command is Meta
      mac-option-modifier 'super	; alt/option is Super
      mac-function-modifier 'none)	; reserve fn for OSX

;;; Default settings

(setq-default
 blink-cursor-mode -1            ; no blinking
 ring-bell-function #'ignore         ; no ringing
 inhibit-startup-screen t            ; no startup screen
 initial-scratch-message ""          ; no message in the scratch buffer
 cursor-in-non-selected-windows nil  ; hide the cursor in inactive windows
 delete-by-moving-to-trash t         ; delete files to trash
 fill-column 80                      ; set width for modern displays
 help-window-select t                ; focus new help windows when opened
 indent-tabs-mode nil                ; stop using tabs to indent
 tab-width 4                         ; but set their width properly
 left-margin-width 0                 ; no left margin
 right-margin-width 0                ; no right margin
 recenter-positions '(12 top bottom) ; set re-centering positions
 scroll-conservatively 1000          ; never recenter point while scrolling
 sentence-end-double-space nil       ; single space after a sentence end
 require-final-newline t             ; require a newline at file end
 show-trailing-whitespace nil        ; don't display trailing whitespaces by default
 uniquify-buffer-name-style 'forward ; uniquify buffer names correctly
 window-combination-resize t         ; resize windows proportionally
 frame-resize-pixelwise t            ; resize frames by pixel (don't snap to char)
 history-length 1000                 ; store more history
 use-dialog-box nil)                 ; don't use dialogues for mouse imput

;; Miscellaneous settings
(fset 'yes-or-no-p 'y-or-n-p)                      ; replace yes/no prompts with y/n
(fset 'display-startup-echo-area-message #'ignore) ; no startup message in the echo area
(delete-selection-mode 1)                          ; replace region when inserting text
(put 'downcase-region 'disabled nil)               ; enable downcase-region
(put 'upcase-region 'disabled nil)                 ; enable upcase-region
(global-hl-line-mode)                              ; highlight the current line
(line-number-mode)                                 ; display line number in the mode line
(column-number-mode)                               ; display column number in the mode line

;;; Basic UI

;; Disable tool bar, scroll bar, and menu bar.
;;
;; Note: menu bar cannot be disabled on OSX, so only remove it if
;; we're not on a Mac (and it's enabled).
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and (not ad:is-a-mac-p) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

;; Use Emacs' builtin line numbering
(gsetq-default display-line-numbers 'visual ; vim-style line numbers
           display-line-numbers-widen t ; disregard any narrowing
           display-line-numbers-current-absolute t) ; display absolute number of current line

(defun ad:relative-line-numbers ()
  (setq-local display-line-numbers 'visual))

(defun ad:absolute-line-numbers ()
  (setq-local display-line-numbers t))

;; Switch to absolute line numbers in insert state
(general-add-hook 'evil-insert-state-entry-hook #'ad:absolute-line-numbers)
(general-add-hook 'evil-insert-state-exit-hook #'ad:relative-line-numbers)

;; Bedazzle the current line number
;; TODO handle this for dark/light themes
(custom-set-faces
 '(line-number-current-line ((t :weight bold :foreground "#b58900"))))

(use-package no-littering
  :config
  ;; Exclude no-littering files from 'recentf'
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)
  ;; Version backups
  (gsetq create-lockfiles nil           ; don't create lockfiles
         delete-old-verisons t          ; don't ask before deleting old backups
         version-control t              ; use version control for backups
         kept-new-versions 10           ; keep 10 newest versions
         kept-old-versions 4            ; keep 4 oldest versions
         vc-make-backup-files)          ; backup files under vc too
  ;; Don't let customization use my init.el file
  (gsetq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (general-add-hook 'after-init-hook
                    (lambda () (load custom-file 'noerror 'nomessage))))

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
  (general-unbind default-text-scale-mode-map
    "C-M--"
    "C-M-="
    "C-M-0")

  (default-text-scale-mode 1))

;;; Colors & Themes

;; Disable old color theme when switching to new color theme
(defun ad:disable-themes (&rest _)
  "Disable all currently active color themes."
  (mapc #'disable-theme custom-enabled-themes))

(general-add-advice 'load-theme :before #'ad:disable-themes)

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

;;; File & Directory handling

(use-package dired
  :ensure nil
  :general ('normal "-" #'counsel-dired-jump)
  :gfhook (nil #'auto-revert-mode)      ; automatically refresh
  :config
  ;; Basic settings
  (gsetq dired-auto-revert-buffer t
         dired-listing-switches "-lha"
         dired-recursive-copies 'always
         dired-dwim-target t)

  ;; Bedazzle 'ls' if we're using a suitable GNU version
  (if ad:is-a-mac-p
      (when (executable-find "gls")
        (gsetq insert-directory-program "gls"
               dired-listing-switches "-lha --group-directories-first"))
    ;; Assume we're on a GNU-compatible system
    (gsetq dired-listing-switches "-lha --group-directories-first")))

(use-package dired-x
  :ensure nil
  :after dired
  :ghook ('dired-mode-hook #'dired-omit-mode)
  :config
  ;; Don't tell me when you're omitting files
  (gsetq dired-omit-verbose nil))

(use-package ignoramus
  :config
  ;; Ignore a few additional things
  (dolist (name '("company-statistics-cache.el"
                  ".metals"
                  ".bloop"))
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (ignoramus-setup))

;;; Version control

(use-package magit
  :defer t
  :general
  ('normal 'override "S" #'magit-status)
  (general-t
    "gs" #'magit-status
    "gl" #'magit-log-all
    "gL" #'magit-log-buffer-file
    "gc" #'magit-commit
    "gp" #'magit-push
    "gf" #'magit-pull
    "gb" #'magit-blame)
  :config
  ;; Basic settings
  (gsetq magit-save-repository-buffers 'dontask
     magit-refs-show-commit-count 'all
     magit-branch-prefer-remote-upstream '("master")
     magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
     magit-revision-show-gravatars nil)

  ;; Show fine-grained diffs in hunks
  (gsetq-default magit-diff-refine-hunk t)

  ;; Set Magit's repository directories for `magit-list-repositories', based on
  ;; Projectile's known projects. This also has effects on `magit-status' in
  ;; "potentially surprising ways". Initialize after Projectile loads, and every
  ;; time we switch projects (we may switch to a previously unknown project).
  (defun ad:set-magit-repository-directories-from-projectile-known-projects ()
    "Set `magit-repository-directories' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      (setq magit-repository-directories
            ;; Strip trailing slashes from project-dirs, since Magit adds them
            ;; again. Double trailing slashes break presentation in Magit
            (mapcar #'directory-file-name project-dirs))))

  (with-eval-after-load 'projectile
    (ad:set-magit-repository-directories-from-projectile-known-projects))

  (general-add-hook
   'projectile-switch-project-hook
   #'ad:set-magit-repository-directories-from-projectile-known-projects))

(use-package evil-magit
  :after evil magit)

(use-package git-timemachine
  :general (general-t "gt" #'git-timemachine))

;;; Completion

(use-package flx)                       ; used by ivy
(use-package smex)                      ; used by counsel

(use-package ivy
  :general (general-spc "f" #'ivy-switch-buffer)
  :config
  ;; Basic settings
  (gsetq ivy-use-virtual-buffers t
         ivy-initial-inputs-alist nil
         ivy-count-format "")

  ;; Enable fuzzy searching everywhere*
  ;;
  ;; *not everywhere
  (gsetq ivy-re-builders-alist
         '((swiper            . ivy--regex-plus)    ; convert spaces to '.*' for swiper
           (ivy-switch-buffer . ivy--regex-plus)    ; and buffer switching
           (counsel-rg        . ivy--regex-plus)    ; and ripgrep
           (t                 . ivy--regex-fuzzy))) ; go fuzzy everywhere else

  (ivy-mode 1))

(use-package counsel
  :general
  ;; Replace standard 'evil-ex-search-forward' with swiper
  ('normal "/" #'counsel-grep-or-swiper)
  ;; Remap standard commands to their counsel analogs
  (general-def
    [remap execute-extended-command] #'counsel-M-x
    [remap find-file]                #'counsel-find-file
    [remap describe-bindings]        #'counsel-descbinds
    [remap describe-face]            #'counsel-describe-face
    [remap describe-function]        #'counsel-describe-function
    [remap describe-variable]        #'counsel-describe-variable
    [remap info-lookup-symbol]       #'counsel-info-lookup-symbol
    [remap completion-at-point]      #'counsel-company
    [remap org-goto]                 #'counsel-org-goto)
  :config (counsel-mode 1))

(use-package swiper
  :general ([remap isearch-forward] #'swiper)
  :init (gsetq swiper-goto-start-of-match t))

(use-package prescient
  :config (prescient-persist-mode))

(use-package ivy-prescient
  :after ivy
  :demand t
  :config (ivy-prescient-mode))

;;; Project management

(use-package projectile
  :general
  (general-spc
    "P" #'projectile-find-file-in-known-projects
    "c" #'projectile-switch-project
    "D" #'projectile-dired)
  :config
  ;; Basic settings
  (gsetq projectile-enable-caching t
         projectile-find-dir-includes-top-level t
         projectile-switch-project-action #'projectile-dired
         projectile-indexing-method 'alien
         projectile-completion-system 'ivy)

  ;; Cleanup dead projects when idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (projectile-mode))

(use-package counsel-projectile
  :general
  (general-spc
    "/" #'ad:counsel-projectile-rg
    "p" #'counsel-projectile-find-file)
  :config
  (gsetq counsel-projectile-sort-files t)

  ;; Make 'counsel-projectile-rg' work outside projects
  (defun ad:counsel-projectile-rg ()
    "Call `counsel-projectile-rg' if in a project, and `counsel-rg' otherwise."
    (interactive)
    (if (projectile-project-p)
        (counsel-projectile-rg)
      (counsel-rg)))

  (counsel-projectile-mode))

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
