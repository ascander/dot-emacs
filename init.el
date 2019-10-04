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

;;; General and friends

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

(use-package which-key
  :defer 5
  :init
  (gsetq which-key-idle-delay 0.4
         which-key-idle-secondary-delay 0.2
         which-key-sort-order 'which-key-key-order-alpha
         which-key-max-display-columns 6
         which-key-add-column-padding 2
         which-key-replacement-alist '(((nil . "Prefix Command") . (nil . "prefix"))
                                       ((nil . "\\`\\?\\?\\'") . (nil . "λ"))
                                       ((nil . "magit-") . (nil . "git-"))))

  ;; No line numbers in `which-key' buffers, please.
  (defun ad:disable-line-numbers-local ()
    "Disables line numbers locally."
    (gsetq display-line-numbers nil))

  (general-add-hook 'which-key-init-buffer-hook #'ad:disable-line-numbers-local)
  :config (which-key-mode 1))

;;; Evil and friends

(use-package evil
  :init
  (gsetq evil-want-keybinding nil     ; don't load evil bindings for other modes
     evil-overriding-maps nil         ; no maps should override evil maps
     evil-search-module 'evil-search  ; use evil-search instead of isearch
     ;; This setting is not respected for 'n/N' searches.
     ;;
     ;; NOTE: persistent highlighting after a search can be cleared with the
     ;; ':nohlsearch' (evil-ex-nohighlight) command.
     ;;
     ;; See: https://github.com/emacs-evil/evil/pull/1128/commits/68d4eb382fe25db74dd1c5f4ddea00ff249ca254
     evil-ex-search-persistent-highlight nil ; no persistent highlighting after search
     evil-want-Y-yank-to-eol t)              ; Y like D
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

(use-package with-editor
  :defer t
  :gfhook #'evil-insert-state
  :config
  (general-def 'normal with-editor-mode-map
    "RET" #'with-editor-finish
    "q" #'with-editor-cancel))

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

(defun ad:disable-line-numbers-local ()
  (setq-local display-line-numbers nil))

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
         delete-old-versions t          ; don't ask before deleting old backups
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

;; Set up ligatures by way of `prettify-symbols-alist'
(defun ad:setup-ligatures ()
  "Append Iosevka ligatures to `prettify-symbols-alist'."
  (gsetq prettify-symbols-alist
         (append prettify-symbols-alist
                 '(
                  ;; Rightwards arrows
                  ("->"   . ?)
                  ("=>"   . ?)
                  ("->>"  . ?)
                  ("=>>"  . ?)
                  ("-->"  . ?)
                  ("==>"  . ?)
                  ("--->" . ?)
                  ("===>" . ?)
                  ("->-"  . ?)
                  ("=>="  . ?)
                  (">-"   . ?)
                  (">>-"  . ?)
                  (">>="  . ?)
                  ("~>"   . ?⤳)

                  ;; Leftwards arrows
                  ("<-"   . ?)
                  ("<<-"  . ?)
                  ("<<="  . ?)
                  ("<--"  . ?)
                  ("<=="  . ?)
                  ("<---" . ?)
                  ("<===" . ?)
                  ("-<-"  . ?)
                  ("=<="  . ?)
                  ("-<"   . ?)
                  ("=<"   . ?)
                  ("-<<"  . ?)
                  ("=<<"  . ?)

                  ;; Bidirectional arrows
                  ("<->"    . ?)
                  ("<=>"    . ?)
                  ("<-->"   . ?)
                  ("<==>"   . ?)
                  ("<--->"  . ?)
                  ("<===>"  . ?)
                  ("<---->" . ?)
                  ("<====>" . ?)

                  ;; Colons
                  ("::"  . ?)
                  (":::" . ?)

                  ;; Logical
                  ("/\\" . ?)
                  ("\\/" . ?)

                  ;; Comparison operators
                  (">="  . ?)
                  ("<="  . ?)

                  ;; Equality/inequality
                  ("=="    . ?)
                  ("!="    . ?)
                  ("==="   . ?)
                  ("!=="   . ?)
                  ("!=="   . ?)
                  ("=!="   . ?)        ; Cats uses a different sequence

                  ;; HTML comments
                  ("<!--"  . ?)
                  ("<!---" . ?)))))

(defun ad:refresh-prettify-symbols-mode ()
  "Toggle `prettify-symbols-mode' explicitly."
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))

;; Hooks for modes in which to use ligatures
(mapc (lambda (hook)
        (general-add-hook hook (lambda ()
                                 (ad:setup-ligatures)
                                 (ad:refresh-prettify-symbols-mode))))
      '(text-mode-hook
        prog-mode-hook))

;; Enable `prettify-symbols-mode'
(global-prettify-symbols-mode +1)

;; Unprettify at right edge
(gsetq prettify-symbols-unprettify-at-point 'right-edge)

;;; Colors & Themes

;; Distinguish evil state by cursor shape/color
;; TODO advise `load-theme' to set cursor colors per theme
(gsetq evil-mode-line-format nil
       evil-normal-state-cursor '(box "#839496")
       evil-motion-state-cursor '(box "#b58900")
       evil-insert-state-cursor '(bar "#268bd2")
       evil-emacs-state-cursor  '(bar "#d33682")
       evil-visual-state-cursor '(box "#d33682"))

;; Disable old color theme when switching to new color theme
(defun ad:disable-themes (&rest _)
  "Disable all currently active color themes."
  (mapc #'disable-theme custom-enabled-themes))

(general-add-advice 'load-theme :before #'ad:disable-themes)

(use-package solarized-theme            ; I always come back to you
  :init
  ;; Basic settings - disprefer bold and italics, use high contrast
  (gsetq solarized-use-variable-pitch nil
    solarized-use-less-bold t
    solarized-use-more-italic nil
    solarized-distinct-doc-face t
    solarized-emphasize-indicators t
    solarized-high-contrast-mode-line nil)
  ;; Avoid all font size changes
  (gsetq solarized-height-minus-1 1.0
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

;;; Mode line

(use-package moody
  :init
  ;; Advise `load-theme' to set mode-line face attributes correctly.
  (defun ad:set-mode-line-attributes (&rest _)
    "Unset the ':box' attribute for the `mode-line' face, and make ':overline'
and ':underline' the same value."
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line          nil :overline  line)
      (set-face-attribute 'mode-line-inactive nil :overline  line)
      (set-face-attribute 'mode-line-inactive nil :underline line)
      (set-face-attribute 'mode-line          nil :box       nil)
      (set-face-attribute 'mode-line-inactive nil :box       nil)))

  (general-add-advice #'load-theme :after #'ad:set-mode-line-attributes)
  :config
  (gsetq x-underline-at-descent-line t
         moody-slant-function #'moody-slant-apple-rgb)

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions
  :after moody
  :init
  (gsetq minions-mode-line-lighter "ʕ•ᴥ•ʔ"
         minions-mode-line-delimiters '("" . ""))
  (minions-mode 1))

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

(use-package autorevert
  :init
  (gsetq auto-revert-verbose nil                ; autorevert quietly
         global-auto-revert-non-file-buffers t) ; and in dired, too

  ;; Notifications aren't used on OSX
  (when ad:is-a-mac-p
    (gsetq auto-revert-use-notify nil))
  :config (global-auto-revert-mode 1))

;;; Windows and buffers

(use-package ace-window
  :general (general-t "w" #'ace-window)
  :config
  ;; Basic settings
  (gsetq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
         aw-scope 'frame))

;; Bindings for window movement/splitting

(use-package windmove
  :config (gsetq windmove-wrap-around t))

(use-package winner
  :general
  (general-t
    "u" #'winner-undo
    "U" #'winner-redo)
  :config (winner-mode))

(defun ad:kill-this-buffer ()
  "Call `kill-this-buffer' without menu bar interaction."
  (interactive)
  (if (minibufferp)
      (abort-recursive-edit)
    (kill-buffer (current-buffer))))

(defun ad:kill-buffer-delete-window ()
  "Kill the current buffer and delete its window."
  (interactive)
  (ad:kill-this-buffer)
  (delete-window))

(defun ad:delete-other-windows ()
  "Make the current window the only one."
  (interactive)
  (if (eq (count-windows) 1)
      (winner-undo)
    (delete-other-windows)))

(defun ad:vsplit ()
  "Split the window vertically and switch to the new window."
  (interactive)
  (split-window-vertically)
  (other-window 1))

(defun ad:hsplit ()
  "Split the window horizontally and switch to the new window."
  (interactive)
  (split-window-horizontally)
  (other-window 1))

(general-t
  "h" #'windmove-left
  "j" #'windmove-down
  "k" #'windmove-up
  "l" #'windmove-right
  "-" #'ad:vsplit
  "'" #'ad:hsplit
  "q" #'ad:kill-this-buffer
  "d" #'delete-window
  "D" #'ad:kill-buffer-delete-window
  "." #'ad:delete-other-windows)

;;; Org mode and friends

(use-package org
  :ensure org-plus-contrib
  :pin org
  :general
  (general-spc
    "c" #'org-capture
    "a" #'org-agenda)
  :config
  ;; Set locations of org directory, agenda files, default notes file
  (defun ad:expand-org-file (file)
    (concat (file-name-as-directory org-directory) file))

  (gsetq org-directory (expand-file-name "~/org")
         org-default-notes-file (ad:expand-org-file "refile.org")
         org-archive-location (concat (ad:expand-org-file "archive.org") "::* From %s")
         org-agenda-files (mapcar #'ad:expand-org-file
                                  '("work.org"
                                    "home.org"
                                    "refile.org"
                                    "reminders.org"
                                    "emacs.org")))

  ;; Default settings
  (gsetq org-src-fontify-natively t
         org-log-done 'time
         org-use-fast-todo-selection t
         org-startup-truncated nil
         org-tags-column -80
         org-enable-priority-commands nil
         org-reverse-note-order t)

  ;; Navigate by headings, using Ivy
  (gsetq org-goto-interface 'outline-path-completion
         org-outline-path-complete-in-steps nil)

  ;; Enable easy templates for src blocks
  (require 'org-tempo)

  ;; Enable habits
  (require 'org-habit)

  ;; Allow `electric-pair-mode' to recognize paired delimiters in org buffers
  ;; See: https://emacs.stackexchange.com/questions/17284/adding-tilde-to-electric-pairs-in-org-mode
  (general-add-hook 'org-mode-hook
                    #'(lambda ()
                        (modify-syntax-entry ?/ "$/" org-mode-syntax-table)
                        (modify-syntax-entry ?= "$=" org-mode-syntax-table)
                        (modify-syntax-entry ?~ "$~" org-mode-syntax-table)))

  ;; TODO task states
  (gsetq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

  ;; Task state settings
  (gsetq org-enforce-todo-dependencies t
         org-enforce-todo-checkbox-dependencies t
         org-treat-S-cursor-todo-selection-as-state-change nil)

  ;; Tags based on state triggers; used for filtering tasks in agenda views
  ;;
  ;; Triggers break down into the following rules:
  ;;
  ;;   - moving a task to CANCELLED adds the CANCELLED tag
  ;;   - moving a task to WAITING adds the WAITING tag
  ;;   - moving a task to HOLD adds the WAITING and HOLD tags
  ;;   - moving a task to a done state removes the WAITING and HOLD tags
  ;;   - moving a task to TODO removes WAITING, CANCELLED, and HOLD tags
  ;;   - moving a task to NEXT removes WAITING, CANCELLED, and HOLD tags
  ;;   - moving a task to DONE removes WAITING, CANCELLED, and HOLD tags
  (gsetq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING" . t) ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

  ;; Org capture templates
  (gsetq org-capture-templates
        '(("t" "Todo" entry (file "~/org/refile.org")
           "* TODO %i%?")
          ("n" "Note" entry (file "~/org/refile.org")
           "* %i%? :NOTE:\n %U")
          ("m" "Meeting" entry (file "~/org/refile.org")
           "* MEETING with %? :MEETING:\n %U")
          ("d" "Deadline" entry (file "~/org/reminders.org")
           "* TODO %i%?\n DEADLINE:%T")))

  ;; Use sensible keybindings for capture buffers
  (general-def 'normal org-capture-mode-map
    "RET" #'org-capture-finalize
    "q" #'org-capture-kill
    "r" #'org-capture-refile)

  ;; Display the right `header-line-format' as well
  (general-add-hook
   'org-capture-mode-hook
   #'(lambda () (gsetq-local header-line-format
                        "Capture buffer. Finish 'RET', refile 'r', abort 'q'.")))

  ;; Refile targets include this file and any agenda file - up to 5 levels deep
  (gsetq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5)))

  ;; Switch to insert state when capturing
  (general-add-hook 'org-capture-mode-hook #'evil-insert-state)
  (general-add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

  ;; Include the filename in refile target paths; this allows refiling to the
  ;; top level of a target
  (gsetq org-refile-use-outline-path 'file)

  ;; Allow refiling to create parent tasks with confirmation
  (gsetq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Do not use hierarchical steps in completion, since we use Ivy
  (gsetq org-outline-path-complete-in-steps nil)

  ;; Display agenda as the only window, but restore the previous configuration
  ;; afterwards; let's be polite.
  (gsetq org-agenda-window-setup 'only-window
         org-agenda-restore-windows-after-quit t)

  ;; Setting `org-agenda-tags-column' to 'auto' doesn't take line numbers into
  ;; account, so if you have line numbers enabled in an org agenda buffer, the
  ;; tags wrap and make things look very ugly.
  (general-add-hook 'org-agenda-mode-hook #'ad:disable-line-numbers-local)

  ;; Do not dim blocked tasks
  (gsetq org-agenda-dim-blocked-tasks nil)

  ;; Just use a newline to separate blocks
  (gsetq org-agenda-block-separator "")

  ;; Compact agenda blocks (disabled)
  (gsetq org-agenda-compact-blocks nil))

(use-package org-ql
  :after org)

;; Custom Org agenda building commands. This is listed separately for a couple
;; of reasons:
;;
;;   1. To break up a long ':config' section for org-mode
;;   2. To force loading of functions in 'org-ql-search.el'
;;
;; Startup time is not adversely impacted, since loading of 'org' + 'org-ql' is
;; deferred until eg. invoking `org-capture' or `org-agenda'. I'd love a better
;; solution here, but a working setup is .9 of the law.
(general-with-package 'org
  ;; Enable agenda building commands
  (require 'org-ql-search)

  ;; Test building an agenda
  (gsetq org-agenda-custom-commands
         '(("w" "Work Agenda"
            ((agenda)
             ;; Followed by next tasks
             (org-ql-block '(and (todo "NEXT")
                                 (not (scheduled))
                                 (tags "@work"))
                           ((org-ql-block-header "Next Tasks:")))
             (org-ql-block '(and (todo "WAITING")
                                 (not (scheduled))
                                 (tags "@work"))
                           ((org-ql-block-header "Waiting on:")))
             (org-ql-block '(and (todo)
                                 (tags "REFILE"))
                           ((org-ql-block-header "Refile:"))))))))

(use-package org-bullets
  :ghook 'org-mode-hook)

(use-package evil-org
  :after evil org
  :config
  ;; TODO can these be put in ':ghook' and ':gfhook' sections?
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook #'(lambda () (evil-org-set-key-theme)))

  ;; Evil bindings in the agenda, too
  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Version control

(use-package magit
  :defer t
  :general
  ('normal 'override "S" #'magit-status)
  (general-t
    "g"  #'(:ignore t :which-key "Git")
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

  ;; Keybindings
  (general-def ivy-minibuffer-map
    "<escape>" #'minibuffer-keyboard-quit ; the natural choice
    "<next>" #'ivy-scroll-up-command      ; default, here for documentation
    "<prior>" #'ivy-scroll-down-command   ; same here
    "C-j" #'ivy-next-history-element      ; repeat command with next element
    "C-k" #'ivy-previous-history-element  ; repeat command with prev element
    "C-'" #'ivy-avy)                      ; pick a candidate using avy

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
  ;; Goto org headings
  (general-m org-mode-map
    "j" #'counsel-org-goto)
  ;; Load themes
  (general-t
    "t" #'counsel-load-theme)
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

(use-package ivy-rich
  :after ivy counsel
  :init
  ;; Align virtual buffers, and abbreviate paths
  (gsetq ivy-virtual-abbreviate 'full
         ivy-rich-path-style 'abbrev
         ivy-rich-switch-buffer-align-virtual-buffer t)
  :config (ivy-rich-mode 1))

(use-package yasnippet
  :defer 3
  :general
  (general-def help-map
    "y" #'yas-describe-tables)
  :ghook ('prog-mode-hook #'yas-minor-mode)
  :config
  ;; Never expand snippets in normal state
  (general-def 'normal yas-minor-mode-map
    [remap yas-expand] #'ignore)

  (yas-reload-all))

(use-package yasnippet-snippets
  :after yasnippet)

;;; Project management

(use-package projectile
  :general
  (general-spc
    "P" #'projectile-find-file-in-known-projects
    "r" #'projectile-switch-project
    "v" #'projectile-invalidate-cache
    "d" #'projectile-dired)
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

;;; General programming

(use-package electric
  :ensure nil
  :init (electric-pair-mode 1)
  :config
  ;; Disable electric pairing in minibuffer
  (gsetq electric-pair-inhibit-predicate #'(lambda (_) (minibufferp))))

(use-package paren
  :ensure nil
  :init (show-paren-mode 1))

(use-package evil-surround
  :init (global-evil-surround-mode 1))

(use-package rainbow-delimiters
  :defer t
  :ghook 'prog-mode-hook 'text-mode-hook)

(use-package company
  :init (global-company-mode)
  :config
  ;; Basic settings
  (gsetq company-idle-delay 0.2
         company-minimum-prefix-length 2
         company-tooltip-align-annotations t
         company-show-numbers t)

  ;; Complete, using the current selection
  (general-def company-active-map
    "C-;" #'company-complete-selection)

  ;; Add YASnippet support for all company backends
  ;; See: https://github.com/syl20bnr/spacemacs/pull/179
  (defun ad:company-backend-with-yas (backends)
    (if (and (listp backends) (memq 'company-yasnippet backends))
        backends
      (append (if (consp backends)
                  backends
                (list backends))
              '(:with company-yasnippet))))

  ;; Add YASnippet to all backends
  (gsetq company-backends
         (mapcar #'ad:company-backend-with-yas company-backends)))

;; Use prescient instead of company-statistics for smrts
(use-package company-prescient
  :after company
  :demand t
  :config
  (company-prescient-mode))

(use-package flycheck
  :ghook ('after-init-hook #'global-flycheck-mode)
  :config
  ;; Basic settings
  (gsetq flycheck-display-errors-delay 0.4)
  ;; Get me outta here
  (general-def 'normal flycheck-error-list-mode
    "q" #'quit-window))

(general-with-package 'prog-mode
  (general-m prog-mode-map
    "j" #'flycheck-next-error
    "k" #'flycheck-previous-error
    "E" #'flycheck-list-errors))

(use-package lsp-mode
  :commands lsp
  :init
  ;; Only enable LSP for these languages/modes
  (general-add-hook 'scala-mode-hook #'lsp)
  :config
  ;; Basic settings
  (gsetq lsp-prefer-flymake nil
         lsp-metals-server-command "metals"
         lsp-response-timeout 20)

  (general-def 'normal lsp-mode-map
    "N" #'lsp-describe-thing-at-point
    "RET" #'lsp-find-definition)

  (general-m lsp-mode-map
    "i" #'lsp-goto-implementation       ; donut work on metals
    "D" #'lsp-find-declaration          ; same
    "x" #'lsp-find-references
    "r" #'lsp-rename                    ; this'n too
    "=" #'lsp-format-buffer)

  (require 'lsp-clients))

(use-package lsp-ui
  :ghook ('lsp-mode-hook #'lsp-ui-mode)
  :init
  ;; Show information only when I ask for it, thanks
  (gsetq-default lsp-ui-sideline-enable nil
                 lsp-ui-doc-enable nil))

(use-package company-lsp
  :after company lsp-mode
  :config (add-to-list 'company-backends 'company-lsp)
  :custom
  (company-lsp-async 1)
  (company-lsp-enable-snippet t))

;;; Major modes

;; Git

(use-package git-commit
  :defer t
  :init
  (gsetq
   git-commit-usage-message
   "Type 'RET' to finish, 'q' to cancel, and \\[git-commit-prev-message] and \\[git-commit-next-message] to recover older messages")
  :config
  ;; Remove style conventions
  (general-remove-hook 'git-commit-finish-query-functions
                       #'git-commit-check-style-conventions))

(use-package gitconfig-mode
  :defer t)

(use-package gitignore-mode
  :defer t)

(use-package gitattributes-mode
  :defer t)

;; Lisp/Emacs Lisp

(use-package elisp-mode
  :ensure nil
  :general
  (general-m emacs-lisp-mode-map
    "b" #'eval-buffer
    "r" #'eval-region
    "f" #'eval-defun)

  (general-def 'normal emacs-lisp-mode-map
    "RET" #'xref-find-definitions
    "<S-return>" #'pop-tag-mark)
  :config
  (gsetq emacs-lisp-docstring-fill-column 80))

;; Markdown

(use-package vmd-mode)

(use-package markdown-mode
  :config
  (general-m markdown-mode-map
    "p" #'vmd-mode))

;; Scala

(use-package scala-mode
  :defer t
  :config
  ;; Indentation preferences
  (gsetq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy
        scala-indent:use-javadoc-style t)

  ;; Insert newline in a multiline comment should insert an asterisk
  (defun ad|scala-mode-newline-comments ()
    "Insert a leading asterisk in multiline comments, when hitting 'RET'."
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (define-key scala-mode-map (kbd "RET") #'ad|scala-mode-newline-comments)

  (general-m scala-mode-map
    "b" #'lsp-metals-build-import
    "c" #'lsp-metals-build-connect
    "d" #'lsp-metals-doctor-run))

(use-package sbt-mode
  :commands sbt-start sbt-command
  :config
  ;; Don't pop up SBT buffers automatically
  (gsetq sbt:display-command-buffer nil)

  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

;;; Coda

(use-package profile-dotemacs
  :load-path "lisp"
  :ensure nil
  :init
  (gsetq profile-dotemacs-low-percentage 1))

;; Display timing information in '*Messages*' buffer
(add-hook 'emacs-startup-hook
      (lambda ()
        (message "Emacs ready in %s with %d garbage collections."
             (format "%.2f seconds"
                 (float-time
                  (time-subtract after-init-time before-init-time))) gcs-done)))

(provide 'init)
;;; init.el ends here
