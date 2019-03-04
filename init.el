;;; init.el --- My Emacs configuration               -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ascander Dost

;; Author: Ascander Dost <dostinthemachine@gmail.com>
;; Keywords:

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

;;; Code:

;; Constants
(defconst *is-a-mac* (eq system-type 'darwin) "Are we on a mac?")

;; Enter debugger on error, and keep more messages.
(setq debug-on-error t)
(setq message-log-max 10000)

;; Increase GC threshold for faster startup, and set to a more reasonable value
;; after startup.
(let ((normal-gc-cons-threshold (* 20 1024 1024))
      (init-gc-cons-threshold (* 256 1024 1024)))
  (setq gc-cons-threshold init-gc-cons-threshold)
  (add-hook 'after-init-hook
            (lambda () (setq gc-cons-threshold normal-gc-cons-threshold))))

;; Display timing information after startup
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "Emacs ready in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

;;; Package management

(require 'package)
(setq load-prefer-newer t            ; prefer the newest version of a file
      package-enable-at-startup nil) ; explicitly initialize packages

(setq package-archives
      '(("melpa"        . "https://melpa.org/packages/")
        ("melpa-stable" . "https://stable.melpa.org/packages/")
        ("gnu"          . "http://elpa.gnu.org/packages/")
        ("org"          . "https://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package t))
(setq-default use-package-always-ensure t)

(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;;; OS X settings

(when *is-a-mac*
  ;; Modifier keys
  ;;
  ;; NOTE: this mapping assumes the default modifier key mapping for OS X. I've
  ;; additionally added the following mappings:
  ;;
  ;;   Caps Lock (⇪) ➔ Control (^)
  ;;   Return (⏎) ➔ Control (^) when used with another key
  ;;
  ;; Using Karabiner Elements (https://pqrs.org/osx/karabiner/).
  (setq mac-command-modifier 'super     ; Command is Super
        mac-option-modifier 'meta       ; Alt/Option is Meta
        mac-right-option-modifier 'none ; Except on the right, just in case
        mac-function-modifier 'none)    ; Reserve Function for OS X

  ;; Reuse existing frame for opening new files
  (setq ns-pop-up-frames nil))

(use-package exec-path-from-shell       ; Fix PATH on GUI Emacs
  :if *is-a-mac*
  :init
  (setq exec-path-from-shell-check-startup-files nil
        exec-path-from-shell-variables
        '(
          "JAVA_OPTS"                   ; Java options
          "SBT_OPTS"                    ; SBT options
          "METALS_ENABLED"              ; Metals for Scala
          "EMAIL"                       ; User email address
          "PATH"                        ; Executables
          "LANG"                        ; System language
          "LC_CTYPE"                    ; System character set
          ))
  :config
  (exec-path-from-shell-initialize))

(use-package osx-trash                  ; Make deleting behave properly on OS X
  :if *is-a-mac*
  :config
  (osx-trash-setup))

;;; Basic UI settings

;; Disable tool-bar, scroll-bar, and menu-bar. The menu bar cannot be
;; removed on OS X, so only remove it if you're not on a Mac.
(when (fboundp 'tool-bar-mode) (tool-bar-mode -1))
(when (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(when (and (not *is-a-mac*) (fboundp 'menu-bar-mode)) (menu-bar-mode -1))

;; Display line numbers suitable for evil
(setq-default display-line-numbers-type 'visual
              display-line-numbers-current-absolute t
              display-line-numbers-width 4
              display-line-numbers-widen t)
(add-hook 'text-mode-hook #'display-line-numbers-mode)
(add-hook 'prog-mode-hook #'display-line-numbers-mode)

;; Set some sensible defaults
(setq-default
 blink-cursor-mode -1                ; No blinking
 ring-bell-function #'ignore         ; No ringing
 inhibit-startup-screen t            ; No startup screen
 initial-scratch-message ""          ; No message in the scratch buffer
 cursor-in-non-selected-windows nil  ; Hide the cursor in inactive windows
 delete-by-moving-to-trash t         ; Delete files to trash
 fill-column 80                      ; Set width for modern displays
 help-window-select t                ; Focus new help windows when opened
 indent-tabs-mode nil                ; Stop using tabs to indent
 tab-width 4                         ; But set their width properly
 left-margin-width 0                 ; No left margin
 right-margin-width 0                ; No right margin
 recenter-positions '(12 top bottom) ; Set re-centering positions
 scroll-conservatively 1000          ; Never recenter point while scrolling
 select-enable-clipboard t           ; Merge system's and Emacs' clipboard
 sentence-end-double-space nil       ; Single space after a sentence end
 require-final-newline t             ; Require a newline at file end
 show-trailing-whitespace nil        ; Don't display trailing whitespaces by default
 uniquify-buffer-name-style 'forward ; Uniquify buffer names correctly
 window-combination-resize t         ; Resize windows proportionally
 frame-resize-pixelwise t            ; Resize frames by pixel (don't snap to char)
 history-length 1000                 ; Store more history
 use-dialog-box nil)                 ; Don't use dialogues for mouse imput

;; Miscellaneous settings
(fset 'yes-or-no-p 'y-or-n-p)                      ; Replace yes/no prompts with y/n
(fset 'display-startup-echo-area-message #'ignore) ; No startup message in the echo area
(delete-selection-mode 1)                          ; Replace region when inserting text
(put 'downcase-region 'disabled nil)               ; Enable downcase-region
(put 'upcase-region 'disabled nil)                 ; Enable upcase-region
(global-hl-line-mode)                              ; Highlight the current line
(line-number-mode)                                 ; Display line number in the mode line
(column-number-mode)                               ; Display column number in the mode line
(setq confirm-kill-emacs 'y-or-n-p)                ; Because I keep hitting 's-q' accidentally

;; ;; Use ESC as a universal "get me out of here" command
;; (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; Make the Emacs shell ('M-x shell') interactive, and disable echoing each
;; terminal command as it's entered on the command line.
(setq shell-command-switch "-ic")
(add-hook 'comint-mode-hook
          '(lambda () (setq comint-process-echoes t)))

;;; Fonts

(set-face-attribute 'default nil
                    :family "Iosevka Pro" ; custom build of Iosevka with ligatures
                    :height 140
                    :weight 'regular)

(set-face-attribute 'variable-pitch nil
                    :family "Fira Sans"
                    :height 150
                    :weight 'regular)

(use-package default-text-scale         ; Easily adjust font size in all Emacs frames
  :init (default-text-scale-mode 1))

(use-package init-ligatures             ; Sick liggz
  :load-path "lisp"
  :ensure nil)

;;; Color theme and looks

;; Advise the `load-theme' function to disable all existing themes first. This
;; avoids pollution from a past theme that isn't overriden by the current theme.
(defun ad|disable-all-themes (&rest args)
  "Disable all currently active themes. ARGS is ignored."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes))

(advice-add #'load-theme :before #'ad|disable-all-themes)

;; Pretty hydra entry point for theme switching
(when (package-installed-p 'hydra)
  (defhydra hydra-theme-selector (:hint nil :color amaranth :columns 3)
    "Theme"
    ("s" (load-theme 'solarized-dark t) "solarized dark")
    ("m" (load-theme 'material t) "material dark")
    ("DEL" (ad|disable-all-themes) "none")

    ("S" (load-theme 'solarized-light t) "solarized light")
    ("M" (load-theme 'material-light t) "material light")
    ("q" nil "quit" :color blue))

  (bind-keys ("C-c w t" . hydra-theme-selector/body)))

;; Create an `after-load-theme-hook' so that we can set faces after switching
;; themes interactively as well.
;; See: https://github.com/pkkm/.emacs.d/blob/e86c9e541a9b18f40292d32dc431557d0ca3e62b/conf/view/color-theme.el#L5-L9
(defvar after-load-theme-hook nil
  "Hook run after a color theme is loaded using `load-theme'.")

(defadvice load-theme (after run-after-load-theme-hook activate)
  "Run `after-load-theme-hook'."
  (run-hooks 'after-load-theme-hook))

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
  ;; Conditionally load the default theme based on whether we're running the Emacs daemon.
  ;; (if (daemonp)
  ;;     (add-hook 'after-make-frame-functions
  ;;               (lambda (frame)
  ;;                 (select-frame frame)
  ;;                 (load-theme 'solarized-dark t)))
  ;;   (load-theme 'solarized-dark t))
  )

(use-package material-theme             ; Google Material Design theme for Emacs
  :defer t)

(use-package doom-themes                ; DOOM Emacs themes
  :init
  (setq doom-themes-enable-bold nil
        doom-themes-enable-italic nil)
  :config
  (if (daemonp)
      (add-hook 'after-make-frame-functions
                (lambda (frame)
                  (select-frame frame)
                  (load-theme 'doom-city-lights t)))
    (load-theme 'doom-city-lights t)))

(use-package dimmer                     ; Dim buffers other than the current one
  :init
  (setq dimmer-fraction 0.4)
  (add-hook 'after-init-hook #'dimmer-mode))

(use-package stripe-buffer              ; striped backgorund in `dired'
  :disabled t                           ; bad interactions with evil cursor states
  :defer t
  :hook (dired-mode . stripe-listify-buffer))

;;; Modeline improvements

(use-package moody                      ; Tabs and Ribbons for the mode line
  :init
  ;; Advise `load-theme' to set mode-line face attributes correctly
  ;; TODO: link to project README for discussion
  (defun ad|set-mode-line-faces-moody (&rest args)
    "Unset the :box attribute for the `mode-line' face, and make
:overline and :underline the same value."
    (interactive)
    (let ((line (face-attribute 'mode-line :underline)))
      (set-face-attribute 'mode-line          nil :overline  line)
      (set-face-attribute 'mode-line-inactive nil :overline  line)
      (set-face-attribute 'mode-line-inactive nil :underline line)
      (set-face-attribute 'mode-line          nil :box       nil)
      (set-face-attribute 'mode-line-inactive nil :box       nil)))

  (advice-add #'load-theme :after #'ad|set-mode-line-faces-moody)
  :config
  (setq x-underline-at-descent-line t
        moody-slant-function #'moody-slant-apple-rgb)

  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package minions                    ; A minor-mode menu for the mode line
  :after moody
  :config (minions-mode 1))

;;; Package manager and init development

(use-package paradox                    ; Modernizing Emacs' package menu
  :defer t
  :init
  (defconst *paradox-github-token-file*
    (concat user-emacs-directory "site-lisp/private.el")
    "Location of the `paradox-github-token' setting for Paradox.")
  :config
  ;; Basic settings
  (setq paradox-execute-asynchronously t
        paradox-spinner-type 'progress-bar
        paradox-column-width-package 32)

  (load *paradox-github-token-file* :noerror :nomessage)
  (paradox-enable))

;;; File handling

(use-package no-littering               ; Help keep '~/.emacs.d' clean
  :config
  ;; Exclude no-littering files from `recentf'
  (require 'recentf)
  (add-to-list 'recentf-exclude no-littering-var-directory)
  (add-to-list 'recentf-exclude no-littering-etc-directory)

  ;; Version backups
  ;; See: https://github.com/manuel-uberti/.emacs.d/blob/c065a68ee7facf677da8495b628e0f83f1271903/init.el#L89-L93
  (setq create-lockfiles nil
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)

  ;; Include auto-save and backup files
  (setq backup-directory-alist
        `((".*" . ,(no-littering-expand-var-file-name "backup/")))
        auto-save-file-name-transforms
        `((".*" ,(no-littering-expand-var-file-name "auto-save/") t)))

  (setq custom-file (no-littering-expand-etc-file-name "custom.el"))
  (add-hook 'after-init-hook (lambda () (load custom-file 'noerror 'nomessage))))

;; Use UTF-8 wherever possible
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(when (display-graphic-p)
  (setq x-select-request-type '(UTF8_STRING COMPOUND_TEXT TEXT STRING)))

(use-package ffap                       ; Find files at point
  :defer t
  ;; Please stop pinging random hosts! See
  ;; https://github.com/technomancy/emacs-starter-kit/issues/39
  :config (setq ffap-machine-p-known 'reject))

(use-package dired                      ; Edit directories
  :ensure nil
  :defer t
  :bind (:map dired-mode-map
              ("s-." . #'hydra-dired/body))
  :init
  (when-let (gls (and *is-a-mac* (executable-find "gls")))
    (setq insert-directory-program gls))

  ;; Hydra from https://github.com/abo-abo/hydra/wiki/Dired
  (defhydra hydra-dired (:hint nil :color pink)
    "
_+_ mkdir          _v_iew           _m_ark             _(_ details        _i_nsert-subdir    wdired
_C_opy             _O_ view other   _U_nmark all       _)_ omit-mode      _$_ hide-subdir    C-x C-q : edit
_D_elete           _o_pen other     _u_nmark           _l_ redisplay      _w_ kill-subdir    C-c C-c : commit
_R_ename           _M_ chmod        _t_oggle           _g_ revert buf     _e_ ediff          C-c ESC : abort
_Y_ rel symlink    _G_ chgrp        _E_xtension mark   _s_ort             _=_ pdiff
_S_ymlink          ^ ^              _F_ind marked      _._ toggle hydra   \\ flyspell
_r_sync            ^ ^              ^ ^                ^ ^                _?_ summary
_z_ compress-file  _A_ find regexp
_Z_ compress       _Q_ repl regexp

T - tag prefix
"
    ("\\" dired-do-ispell)
    ("(" dired-hide-details-mode)
    (")" dired-omit-mode)
    ("+" dired-create-directory)
    ("=" diredp-ediff)                  ; smart diff
    ("?" dired-summary)
    ("$" diredp-hide-subdir-nomove)
    ("A" dired-do-find-regexp)
    ("C" dired-do-copy)                 ; copy all marked files
    ("D" dired-do-delete)
    ("E" dired-mark-extension)
    ("e" dired-ediff-files)
    ("F" dired-do-find-marked-files)
    ("G" dired-do-chgrp)
    ("g" revert-buffer)                 ; read all directories again (refresh)
    ("i" dired-maybe-insert-subdir)
    ("l" dired-do-redisplay)            ; relist the marked or single directory
    ("M" dired-do-chmod)
    ("m" dired-mark)
    ("O" dired-display-file)
    ("o" dired-find-file-other-window)
    ("Q" dired-do-find-regexp-and-replace)
    ("R" dired-do-rename)
    ("r" dired-do-rsynch)
    ("S" dired-do-symlink)
    ("s" dired-sort-toggle-or-edit)
    ("t" dired-toggle-marks)
    ("U" dired-unmark-all-marks)
    ("u" dired-unmark)
    ("v" dired-view-file)               ; q to exit, s to search, = gets line #
    ("w" dired-kill-subdir)
    ("Y" dired-do-relsymlink)
    ("z" diredp-compress-this-file)
    ("Z" dired-do-compress)
    ("q" nil)
    ("." nil :color blue))
  :config
  ;; Basic dired settings
  (setq dired-auto-revert-buffer t
        dired-listing-switches "-alh"
        dired-recursive-copies 'always
        dired-dwim-target t)

  ;; Use more 'ls' switches if we have them available
  (when (or (memq system-type '(gnu gnu/linux))
            (string= (file-name-nondirectory insert-directory-program) "gls"))
    (setq dired-listing-switches
          (concat dired-listing-switches " --group-directories-first -v"))))

(use-package dired-x                    ; Additional tools for 'dired'
  :ensure nil
  :after dired
  :init
  (add-hook 'dired-mode-hook #'dired-omit-mode)
  :config
  ;; Don't tell me when you're omitting files, and additionally hide some other
  ;; uninteresting files in Dired.
  (setq dired-omit-verbose nil
        dired-omit-files
        (concat dired-omit-files "\\|^.DS_STORE$\\|^.projectile$")))

(use-package ignoramus                  ; Ignore uninteresting files everywhere
  :config
  ;; Ignore some additional files and directories
  (dolist (name '("company-statistics-cache.el"
                  ".ensime"
                  ".ensime_cache"))
    (add-to-list 'ignoramus-file-basename-exact-names name))

  (ignoramus-setup))

(use-package recentf                    ; Save recently visited files
  :init
  (setq recentf-max-saved-items 100     ; Save more recent items
        recentf-max-menu-items 15       ; In the menu, too
        recentf-auto-cleanup 300        ; Cleanup after 300 idle seconds

        ;; Exclude some boring files
        recentf-exclude (list "/\\.git/.*\\'" ; Git contents
                              "/elpa/.*\\'"   ; Package files
                              #'ignoramus-boring-p))
  :config (recentf-mode 1))

(use-package autorevert
  :init
  ;; Basic settings
  (setq auto-revert-verbose nil                ; Autorevert quietly
        global-auto-revert-non-file-buffers t) ; Dired, too

  ;; File notifications are not used on OS X
  (when *is-a-mac*
    (setq auto-revert-use-notify nil))
  :config (global-auto-revert-mode 1))

;; View read-only files
(setq view-read-only t)

;;; Buffer, frame and window settings

;; ;; Always prefer splitting vertically
;; ;;
;; ;; See: https://stackoverflow.com/a/2081978
;; (setq split-height-threshold nil
;;       split-width-threshold nil)

(use-package ibuffer                       ; A better buffer list
  :bind (([remap list-buffers] . ibuffer)  ; C-x C-b
         :map ibuffer-mode-map
         ("s-." . #'hydra-ibuffer-main/body))
  :init
  (defhydra hydra-ibuffer-main (:color pink :hint nil)
    "
^Mark^         ^Actions^         ^View^          ^Select^              ^Navigation^
_m_: mark      _D_: delete       _g_: refresh    _q_: quit             _k_:   ↑    _h_
_u_: unmark    _s_: save marked  _S_: sort       _TAB_: toggle         _RET_: visit
_*_: specific  _a_: all actions  _/_: filter     _o_: other window     _j_:   ↓    _l_
_t_: toggle    _._: toggle hydra _H_: help       C-o other win no-select
"
    ("m" ibuffer-mark-forward)
    ("u" ibuffer-unmark-forward)
    ("*" hydra-ibuffer-mark/body :color blue)
    ("t" ibuffer-toggle-marks)

    ("D" ibuffer-do-delete)
    ("s" ibuffer-do-save)
    ("a" hydra-ibuffer-action/body :color blue)

    ("g" ibuffer-update)
    ("S" hydra-ibuffer-sort/body :color blue)
    ("/" hydra-ibuffer-filter/body :color blue)
    ("H" describe-mode :color blue)

    ("h" ibuffer-backward-filter-group)
    ("k" ibuffer-backward-line)
    ("l" ibuffer-forward-filter-group)
    ("j" ibuffer-forward-line)
    ("RET" ibuffer-visit-buffer :color blue)

    ("TAB" ibuffer-toggle-filter-group)

    ("o" ibuffer-visit-buffer-other-window :color blue)
    ("q" quit-window :color blue)
    ("." nil :color blue))

  (defhydra hydra-ibuffer-mark (:color teal :columns 5
                                       :after-exit (hydra-ibuffer-main/body))
    "Mark"
    ("*" ibuffer-unmark-all "unmark all")
    ("M" ibuffer-mark-by-mode "mode")
    ("m" ibuffer-mark-modified-buffers "modified")
    ("u" ibuffer-mark-unsaved-buffers "unsaved")
    ("s" ibuffer-mark-special-buffers "special")
    ("r" ibuffer-mark-read-only-buffers "read-only")
    ("/" ibuffer-mark-dired-buffers "dired")
    ("e" ibuffer-mark-dissociated-buffers "dissociated")
    ("h" ibuffer-mark-help-buffers "help")
    ("z" ibuffer-mark-compressed-file-buffers "compressed")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-action (:color teal :columns 4
                                         :after-exit
                                         (if (eq major-mode 'ibuffer-mode)
                                             (hydra-ibuffer-main/body)))
    "Action"
    ("A" ibuffer-do-view "view")
    ("E" ibuffer-do-eval "eval")
    ("F" ibuffer-do-shell-command-file "shell-command-file")
    ("I" ibuffer-do-query-replace-regexp "query-replace-regexp")
    ("H" ibuffer-do-view-other-frame "view-other-frame")
    ("N" ibuffer-do-shell-command-pipe-replace "shell-cmd-pipe-replace")
    ("M" ibuffer-do-toggle-modified "toggle-modified")
    ("O" ibuffer-do-occur "occur")
    ("P" ibuffer-do-print "print")
    ("Q" ibuffer-do-query-replace "query-replace")
    ("R" ibuffer-do-rename-uniquely "rename-uniquely")
    ("T" ibuffer-do-toggle-read-only "toggle-read-only")
    ("U" ibuffer-do-replace-regexp "replace-regexp")
    ("V" ibuffer-do-revert "revert")
    ("W" ibuffer-do-view-and-eval "view-and-eval")
    ("X" ibuffer-do-shell-command-pipe "shell-command-pipe")
    ("b" nil "back"))

  (defhydra hydra-ibuffer-sort (:color amaranth :columns 3)
    "Sort"
    ("i" ibuffer-invert-sorting "invert")
    ("a" ibuffer-do-sort-by-alphabetic "alphabetic")
    ("v" ibuffer-do-sort-by-recency "recently used")
    ("s" ibuffer-do-sort-by-size "size")
    ("f" ibuffer-do-sort-by-filename/process "filename")
    ("m" ibuffer-do-sort-by-major-mode "mode")
    ("b" hydra-ibuffer-main/body "back" :color blue))

  (defhydra hydra-ibuffer-filter (:color amaranth :columns 4)
    "Filter"
    ("m" ibuffer-filter-by-used-mode "mode")
    ("M" ibuffer-filter-by-derived-mode "derived mode")
    ("n" ibuffer-filter-by-name "name")
    ("c" ibuffer-filter-by-content "content")
    ("e" ibuffer-filter-by-predicate "predicate")
    ("f" ibuffer-filter-by-filename "filename")
    (">" ibuffer-filter-by-size-gt "size")
    ("<" ibuffer-filter-by-size-lt "size")
    ("/" ibuffer-filter-disable "disable")
    ("b" hydra-ibuffer-main/body "back" :color blue))
  :config
  (setq ibuffer-formats
        '((mark modified read-only vc-status-mini " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " "
                (vc-status 16 16 :left)
                " "
                filename-and-process)
          (mark modified read-only " "
                (name 18 18 :left :elide)
                " "
                (size 9 -1 :right)
                " "
                (mode 16 16 :left :elide)
                " " filename-and-process)
          (mark " " (name 16 -1) " " filename))))

(use-package ibuffer-vc                 ; Group buffers by VC project
  :commands ibuffer-vc-set-filter-groups-by-vc-root
  :hook (ibuffer . (lambda ()
                     (ibuffer-vc-set-filter-groups-by-vc-root)
                     (unless (eq ibuffer-sorting-mode 'alphabetic)
                       (ibuffer-do-sort-by-alphabetic)))))

(use-package ace-window                 ; Fast window switching
  :bind (("s-;" . ace-window))
  :init
  :config
  ;; Set face for `aw-leading-char-face'
  (set-face-attribute 'aw-leading-char-face nil
                      :foreground "deep sky blue"
                      :height 2.0)
  ;; Settings
  (setq aw-keys '(?a ?s ?d ?f ?j ?k ?l)
        aw-dispatch-always t
        aw-dispatch-alist
        '((?x aw-delete-window     "Ace - Delete Window")
          (?c aw-swap-window       "Ace - Swap Window")
          (?n aw-flip-window       "Ace - Flip Window")
          (?v aw-split-window-vert "Ace - Split Vert Window")
          (?h aw-split-window-horz "Ace - Split Horiz Window")
          (?m delete-other-windows "Ace - Maximize Window")
          (?g delete-other-windows)
          (?b balance-windows)
          (?u winner-undo)
          (?r winner-redo)))

  ;; Add hydras for sizing/frames
  (when (package-installed-p 'hydra)
    (defhydra hydra-window-size (:color red)
      "Window size"
      ("h" shrink-window-horizontally "shrink horizontal")
      ("j" shrink-window "shrink vertical")
      ("k" enlarge-window "enlarge vertical")
      ("l" enlarge-window-horizontally "enlarge horizontal"))
    (defhydra hydra-window-frame (:color red)
      "Frame"
      ("f" make-frame "new frame")
      ("x" delete-frame "delete frame"))

    (add-to-list 'aw-dispatch-alist '(?w hydra-window-size/body) t)
    (add-to-list 'aw-dispatch-alist '(?\; hydra-window-frame/body) t)))

(use-package focus-autosave-mode        ; Save buffers when Emacs loses focus
  :config (focus-autosave-mode t))

(use-package winner                     ; Undo/redo window configuration changes
  :config (winner-mode 1))

(use-package windmove                   ; Navigate windows
  :config
  (setq windmove-wrap-around t)

  ;; Set up common MacOS keybindings for window navigation (like in iTerm)
  (global-set-key (kbd "s-[") #'windmove-left)
  (global-set-key (kbd "s-]") #'windmove-right)
  (global-set-key (kbd "s-{") #'windmove-up)
  (global-set-key (kbd "s-}") #'windmove-up))

;; ;; Quicker buffer cycling commands
;; (bind-keys ("s-p" . previous-buffer)    ; TODO: conflicts with projectile command prefix
;;            ("s-n" . next-buffer)
;;            ("s-k" . kill-this-buffer))

;; Split and manage windows easily
(global-set-key (kbd "s-1") (kbd "C-x 1")) ; ⌘-1 kill other windows (keep this one)
(global-set-key (kbd "s-2") (kbd "C-x 2")) ; ⌘-2 split horizontally
(global-set-key (kbd "s-3") (kbd "C-x 3")) ; ⌘-3 split vertically
(global-set-key (kbd "s-0") (kbd "C-x 0")) ; ⌘-0 kill this window

;;; Org

(use-package init-org                   ; The almighty Org mode
  :load-path "lisp"
  :ensure nil)

(use-package ox-reveal                  ; Reveal.js back end for Org export
  :defer t
  :load-path "site-lisp/org-reveal")

(use-package evil-org                   ; Evil bindings for Org mode
  :after evil org
  :config
  (add-hook 'org-mode-hook #'evil-org-mode)
  (add-hook 'evil-org-mode-hook (lambda () (evil-org-set-key-theme)))

  (require 'evil-org-agenda)
  (evil-org-agenda-set-keys))

;;; Project management

(use-package projectile                 ; Project management for Emacs
  :defer 2
  :bind-keymap ("M-p" . projectile-command-map)
  :bind (("M-P" . hydra-projectile/body))
  :init
  (defhydra hydra-projectile (:color teal :columns 4)
    "Projectile"
    ("f"   projectile-find-file                "Find File")
    ("r"   projectile-recentf                  "Recent Files")
    ("z"   projectile-cache-current-file       "Cache Current File")
    ("x"   projectile-remove-known-project     "Remove Known Project")

    ("d"   projectile-find-dir                 "Find Directory")
    ("b"   projectile-switch-to-buffer         "Switch to Buffer")
    ("c"   projectile-invalidate-cache         "Clear Cache")
    ("X"   projectile-cleanup-known-projects   "Cleanup Known Projects")

    ("o"   projectile-multi-occur              "Multi Occur")
    ("s"   projectile-switch-project           "Switch Project")
    ("k"   projectile-kill-buffers             "Kill Buffers")
    ("q"   nil "Cancel" :color blue))
  :config
  ;; Basic settings
  (setq projectile-completion-system 'ivy
        projectile-enable-caching t
        projectile-find-dir-includes-top-level t
        projectile-switch-project-action #'projectile-dired
        projectile-indexing-method 'alien)

  ;; Location of Projectile data files. This is actually set in `no-littering',
  ;; but because we defer loading of Projectile, those paths are overwritten
  ;; with Projectile defaults.
  (setq projectile-cache-file
        (no-littering-expand-var-file-name "projectile/cache.el"))

  ;; Remove dead projects when Emacs is idle
  (run-with-idle-timer 10 nil #'projectile-cleanup-known-projects)

  (projectile-mode t))

(use-package treemacs                   ; File explorer for Emacs
  :defer t
  :bind
  (:map global-map
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag))
  :init
  (with-eval-after-load 'winum
    (define-key winum-keymap (kbd "M-0") #'treemacs-select-window))
  :config
  (progn
    (setq treemacs-collapse-dirs                 (if (executable-find "python") 3 0)
          treemacs-deferred-git-apply-delay      0.5
          treemacs-display-in-side-window        t
          treemacs-file-event-delay              5000
          treemacs-file-follow-delay             0.2
          treemacs-follow-after-init             t
          treemacs-git-command-pipe              ""
          treemacs-goto-tag-strategy             'refetch-index
          treemacs-indentation                   2
          treemacs-indentation-string            " "
          treemacs-is-never-other-window         nil
          treemacs-max-git-entries               5000
          treemacs-no-png-images                 nil
          treemacs-no-delete-other-windows       t
          treemacs-project-follow-cleanup        nil
          treemacs-persist-file                  (expand-file-name ".cache/treemacs-persist" user-emacs-directory)
          treemacs-recenter-distance             0.1
          treemacs-recenter-after-file-follow    nil
          treemacs-recenter-after-tag-follow     nil
          treemacs-recenter-after-project-jump   'always
          treemacs-recenter-after-project-expand 'on-distance
          treemacs-show-cursor                   nil
          treemacs-show-hidden-files             t
          treemacs-silent-filewatch              nil
          treemacs-silent-refresh                nil
          treemacs-sorting                       'alphabetic-desc
          treemacs-space-between-root-nodes      t
          treemacs-tag-follow-cleanup            t
          treemacs-tag-follow-delay              1.5
          treemacs-width                         35)

    ;; Keep treemacs from showing files that are ignored by git
    (add-to-list 'treemacs-pre-file-insert-predicates #'treemacs-is-file-git-ignored?)
    
    ;; The default width and height of the icons is 22 pixels. If you are
    ;; using a Hi-DPI display, uncomment this to double the icon size.
    ;;(treemacs-resize-icons 44)

    (treemacs-follow-mode t)
    (treemacs-filewatch-mode t)
    (treemacs-fringe-indicator-mode t)
    (pcase (cons (not (null (executable-find "git")))
                 (not (null (executable-find "python3"))))
      (`(t . t)
       (treemacs-git-mode 'deferred))
      (`(t . _)
       (treemacs-git-mode 'simple)))))

(use-package treemacs-evil              ; Evil integration for treemacs
  :after treemacs evil)

(use-package treemacs-projectile        ; Projectile integration for treemacs
  :after treemacs projectile)

(use-package treemacs-icons-dired       ; Treemacs icons in dired buffers
  :after treemacs dired
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit             ; Magit integration for Treemacs
  :after treemacs magit)

;;; Version control

(use-package what-the-commit            ; Random commit message
  :bind (("C-c i w" . what-the-commit-insert)
         ("C-c g w" . what-the-commit)))

(use-package magit                      ; The one and only Git front end
  :bind (("C-c g c" . magit-clone)
         ("C-c g s" . magit-status)
         ("s-G"     . magit-status)
         ("C-c g b" . magit-blame-addition)
         ("C-c g p" . magit-pull-branch))
  :config
  ;; Basic settings
  (setq magit-save-repository-buffers 'dontask
        magit-refs-show-commit-count 'all
        magit-branch-prefer-remote-upstream '("master")
        magit-branch-adjust-remote-upstream-alist '(("origin/master" "master"))
        magit-revision-show-gravatars nil
        magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1)

  ;; Show refined hunks during diffs
  (set-default 'magit-diff-refine-hunk t)

  ;; Set Magit's repo dirs for `magit-status' from Projectile's known projects.
  ;; Initialize the `magit-repository-directories' immediately after Projectile
  ;; was loaded, and update it every time we switched projects, because the new
  ;; project might have been unknown before
  (defun ad|magit-set-repo-dirs-from-projectile ()
    "Set `magit-repo-dirs' from known Projectile projects."
    (let ((project-dirs (bound-and-true-p projectile-known-projects)))
      ;; Remove trailing slashes from project directories, because Magit adds
      ;; trailing slashes again, which breaks the presentation in the Magit
      ;; prompt.
      (setq magit-repository-directories
                     (mapcar #'directory-file-name project-dirs))))
  (with-eval-after-load 'projectile
    (ad|magit-set-repo-dirs-from-projectile))

  (add-hook 'projectile-switch-project-hook
            #'ad|magit-set-repo-dirs-from-projectile)

  ;; Refresh `magit-status' after saving a buffer
  (add-hook 'after-save-hook #'magit-after-save-refresh-status)

  ;; Redirect "git" command to "hub" for interactive use only.
  ;; See: https://github.com/DarwinAwardWinner/dotemacs/blob/8ed1ae8244b5c23965799b81c2371e27217bae48/config.org#magit-itself
  ;;
  ;; Note: This requires configuring the `$EDITOR' environment variable to play
  ;; nice with Emacs running in server mode. For Oh-my-zsh users (also using the
  ;; Emacs plugin) this looks like:
  ;;
  ;; export EDITOR="$HOME/.oh-my-zsh/plugins/emacs/emacsclient.sh --no-wait"
  ;;
  ;; See: https://unix.stackexchange.com/a/9202
  (defvar magit-hub-executable (when (executable-find "hub") "hub")
    "Executable to use for calling hub.")
  (define-advice magit-git-command (:around (orig-fun &rest args) use-hub)
    "Use `hub' instead of `git' if available."
    (let ((magit-git-executable
           (or magit-hub-executable magit-git-executable)))
      (apply orig-fun args))))

(use-package git-commit                 ; git commit message mode
  :defer t
  :config
  ;; Don't check style conventions.
  (remove-hook 'git-commit-finish-query-functions
               #'git-commit-check-style-conventions))

(use-package gitconfig-mode             ; Gitconfig editing
  :defer t)

(use-package gitignore-mode             ; '.gitignore' file editing
  :defer t)

(use-package gitattributes-mode         ; '.gitattributes' file editing
  :defer t)

(use-package git-timemachine            ; Go back in Git time
  :bind (("C-c g t" . git-timemachine)))

(use-package git-link                   ; Create github links from buffers
  :disabled t                           ; Does NOT work with ssh-config hosts
  :config
  (add-to-list 'git-link-remote-alist '("^ghe?" git-link-github)))

;;; Completion

(use-package hydra                      ; Make Emacs bindings that stick around
  :defer t)

(use-package ivy                        ; Generic completion mechanism for Emacs
  :demand t
  :bind (("s-b" . ivy-switch-buffer)
         ("s-B" . ivy-switch-buffer-other-window)
         ("s-r" . ivy-resume))
  :config
  (use-package flx)

  ;; Basic settings
  (setq ivy-use-virtual-buffers t
        ivy-initial-inputs-alist nil
        ivy-count-format "")

  ;; Enable fuzzy searching everywhere except for Swiper
  (setq ivy-re-builders-alist
        '((swiper            . ivy--regex-plus)
          (ivy-switch-buffer . ivy--regex-plus)
          (t                 . ivy--regex-fuzzy)))

  ;; Make the `ivy-current-match' face a bit more distinct
  ;; (set-face-attribute 'ivy-current-match nil :inherit #'warning)
  ;; (add-hook 'after-load-theme-hook
  ;;           '(lambda () (set-face-attribute
  ;;                   'ivy-current-match nil :inherit #'warning)))

  (ivy-mode 1))

(use-package ivy-hydra                  ; A useful hydra for the Ivy minibuffer
  :after (ivy hydra)
  :defer t
  :config
  (bind-key "s-." #'hydra-ivy/body ivy-minibuffer-map))

(use-package ivy-rich
  :after ivy counsel
  :config
  ;; Align virtual buffers, and abbreviate paths
  (setq ivy-virtual-abbreviate 'full
        ivy-rich-path-style 'abbrev)

  (ivy-set-display-transformer 'ivy-switch-buffer
                               'ivy-rich-switch-buffer-transformer)
  (ivy-rich-mode 1))

(use-package counsel                    ; Ivy-enhanced versions of commands
  :after ivy
  :demand t
  :bind (([remap execute-extended-command] . counsel-M-x)
         ("s-P"                            . counsel-M-x) ; familiar command palette keybinding for MacOS
         ([remap find-file]                . counsel-find-file)
         ([remap describe-face]            . counsel-describe-face)
         ([remap describe-function]        . counsel-describe-function)
         ([remap describe-variable]        . counsel-describe-variable)
         ([remap info-lookup-symbol]       . counsel-info-lookup-symbol)
         ([remap completion-at-point]      . counsel-company)
         ([remap org-goto]                 . counsel-org-goto)
         ("C-c f L"                        . counsel-load-library)
         ("C-c f r"                        . counsel-recentf)
         ("C-c i 8"                        . counsel-unicode-char)
         ("C-c r g"                        . counsel-rg)
         ("C-c j t"                        . counsel-imenu)
         ("C-c g L"                        . counsel-git-log))
  :config
  ;; Use Smex ranking of results automatically
  (use-package smex
    :config (smex-initialize))

  ;; Counsel-powered `org-goto' command
  (setq counsel-outline-display-style 'path)

  (counsel-mode 1))

(use-package counsel-projectile         ; Counsel integration with Projectile
  :after (counsel projectile)
  :bind (("s-p" . counsel-projectile-find-file)) ; Find file in current project with
  :config
  ;; TODO: remap `projectile-ag' to `counsel-projectile-rg'
  (counsel-projectile-mode 1))

(use-package swiper                     ; An Ivy-enhanced alternative to isearch
  :after ivy
  :bind (([remap isearch-forward] . swiper)))

;;; Keys and keybindings

(use-package evil                       ; Vim keybindings for Emacs
  :init
  (setq evil-want-integration t
        evil-want-keybinding nil)

  ;; Start things like commit message buffers in insert state
  (add-hook 'with-editor-mode-hook #'evil-insert-state)
  :config (evil-mode 1))

(use-package evil-collection            ; Evil bindings for Emacs modes
  :after evil
  :config (evil-collection-init))

(use-package evil-escape                ; Customizable escape from insert state
  :after evil
  :init (setq-default evil-escape-key-sequence "jk")
  :config (evil-escape-mode 1))

(use-package evil-magit                 ; Evil bindings for Magit
  :after evil magit)

(use-package which-key                  ; Display keybindings based on current command
  :defer 5
  :init
  (setq which-key-idle-delay 0.4
        which-key-sort-order 'which-key-prefix-then-key-order
        which-key-replacement-alist
        '(
          ;; Replacements for how all or part of FUNCTION is replaced when
          ;; `which-key' displays:
          ;;
          ;;     KEY → Function
          ;;
          ;; Eg: after "C-c g" display "s → magit-status" as "s → git-status"
          ((nil . "Prefix Command")            . (nil . "prefix"))
          ((nil . "\\`\\?\\?\\'")              . (nil . "λ"))
          ((nil . "projectile-")               . (nil . "pj-"))
          ((nil . "magit-")                    . (nil . "git-"))
          ((nil . "\\`hydra-\\(.+\\)/body\\'") . (nil . "=|\\1"))
          ((nil . "\\`hydra-\\(.+\\)\\'")      . (nil . "=|\\1"))))
  :config
  (which-key-mode 1))

(use-package free-keys                  ; Show free keybindings in the current buffer
  :defer 5)

;;; Basic editing

;; Common MacOS keybindings; taken from https://github.com/freetonik/castlemacs
(global-set-key (kbd "s-s") 'save-buffer)             ; save
(global-set-key (kbd "s-S") 'write-file)              ; save as
(global-set-key (kbd "s-q") 'save-buffers-kill-emacs) ; quit
(global-set-key (kbd "s-a") 'mark-whole-buffer)       ; select all

;; Use super+h|j|k|l for navigation  instead of C-f|n|p|b
(global-set-key (kbd "s-h") #'left-char)
(global-set-key (kbd "s-j") #'next-line)
(global-set-key (kbd "s-k") #'previous-line)
(global-set-key (kbd "s-l") #'right-char)

;; Common ⌘-[arrow] keybindings for navigation/selection
(global-set-key (kbd "s-<right>") #'move-end-of-line)   ; end of line
(global-set-key (kbd "s-S-<right>") (kbd "C-S-e"))      ; select to end of line
(global-set-key (kbd "s-<left>") #'back-to-indentation) ; beginning (first non-blank) of line
(global-set-key (kbd "s-S-<left>") (kbd "M-S-m"))       ; select to beginning of line

(global-set-key (kbd "s-<up>") #'beginning-of-buffer)   ; first line
(global-set-key (kbd "s-<down>") #'end-of-buffer)       ; last line

;; Killing; note 'M-<backspace>' kills the word backwards
(global-set-key (kbd "s-<backspace>") #'kill-whole-line) ; kill line backwards
(global-set-key (kbd "M-S-<backspace>") #'kill-word)     ; kill word forwards

(use-package writeroom-mode             ; Distraction free editing mode
  :bind (:map writeroom-mode-map
              ("C-M-<" . #'writeroom-decrease-width)
              ("C-M->" . #'writeroom-increase-width)
              ("C-M-=" . #'writeroom-adjust-width)
              ("s-m"   . #'writeroom-toggle-mode-line)))

;; Allow narrowing (disabled by default)
(put 'narrow-to-region 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-defun 'disabled nil)

;;; Pair management

(use-package electric               ; Toggle automatic pairing of brackets, etc.
  :ensure nil
  :init (electric-pair-mode 1))

(use-package evil-surround              ; Surrounding, with Evil
  :config (global-evil-surround-mode 1))

(use-package unfill                     ; The inverse of Emacs' fill
  :defer t)

(use-package undo-tree                  ; Replace the confusing Emacs undo system
  :init
  (setq undo-tree-auto-save-history t
        undo-tree-visualizer-timestamps t
        undo-tree-visualizer-diff t)
  :config
  (global-undo-tree-mode 1)

  ;; Use familiar MacOS keybindings for undo/redo
  (global-set-key (kbd "s-z") 'undo-tree-undo)
  (global-set-key (kbd "s-Z") 'undo-tree-redo))

;;; Programming Settings

;; Use the familiar MacOS keybinding for commenting
(global-set-key (kbd "s-/") #'comment-dwim)

(use-package shell-pop                  ; Use a shell easily on Emacs
  :init
  (setq shell-pop-window-size 40)
  :config
  ;; Set `default-directory' to the projectile root dir, if available. This
  ;; makes shell-pop in an SBT project tolerable
  (add-hook 'shell-pop-in-hook '(lambda ()
                                  (let ((root
                                         (if
                                             (fboundp 'projectile-project-root)
                                             (or (projectile-project-root) default-directory))))
                                    (setq default-directory root))))

  (custom-set-variables
   '(shell-pop-shell-type
     (quote ("term" "*terminal*" (lambda nil (term shell-pop-term-shell)))))
   '(shell-pop-universal-key "M-=")))

;; Make symbols pretty in programming contexts
(when (fboundp 'global-prettify-symbols-mode)
  (add-hook 'prog-mode-hook #'global-prettify-symbols-mode))

(use-package rainbow-delimiters         ; Highlight delimiters by depth
  :defer t
  :hook ((prog-mode . rainbow-delimiters-mode)
         (text-mode . rainbow-delimiters-mode)))

(use-package rainbow-mode               ; Fontify colors in buffers
  :bind (("C-c t r" . rainbow-mode))
  :hook prog-mode
  :init
  ;; Don't highlight color names (just color codes, thanks)
  (setq rainbow-x-colors nil))

(use-package whitespace-cleanup-mode    ; Intelligently clean up whitespace before buffers are saved
  :bind (("C-c x w" . whitespace-cleanup))
  :hook (prog-mode text-mode conf-mode))

(use-package eldoc                      ; Print argument information in the echo area
  :defer t
  :hook (eval-expression-minibuffer-setup . eldoc-mode))

(use-package deadgrep                   ; Fast, beautiful text search
  :bind ("s-F" . deadgrep))             ; MacOS familiar ⌘-shift-f binding

(use-package multiple-cursors           ; Edit text with multiple cursors
  :defer 4
  :bind (("C-c o <SPC>" . mc/vertical-align-with-space)
         ("C-c o a"     . mc/vertical-align)
         ("C-c o e"     . mc/mark-more-like-this-extended)
         ("C-c o h"     . mc/mark-all-like-this-dwim)
         ("C-c o i n"   . mc/insert-numbers)
         ("C-c o i l"   . mc/insert-letters)
         ("C-c o l"     . mc/edit-lines)
         ("C-c o n"     . mc/mark-next-like-this)
         ("C-c o p"     . mc/mark-previous-like-this)
         ("C-c o r"     . vr/mc-mark)
         ("C-c o C-a"   . mc/edit-beginnings-of-lines)
         ("C-c o C-e"   . mc/edit-ends-of-lines)
         ("C-c o C-s"   . mc/mark-all-in-region))
  :config
  ;; Keep preferences in no-littering directory
  (setq mc/list-file (no-littering-expand-var-file-name "mc-lists.el"))

  (setq
   mc/mode-line
   ;; Simplify the MC mode line indicator
   '(:propertize (:eval (concat " " (number-to-string (mc/num-cursors))))
                 face font-lock-warning-face)))

(use-package expand-region              ; Expand region by semantic units
  :ensure t
  :bind (("s-'"  . er/expand-region)
         ("s-\"" . er/contract-region)))

(use-package dumb-jump                  ; Jump to definition dumbly
  :hook ((prog-mode . dumb-jump-mode))
  :bind (("M-g o" . dumb-jump-go-other-window)
         ("M-g j" . dumb-jump-go)
         ("M-g b" . dumb-jump-back)
         ("M-g i" . dumb-jump-go-prompt)
         ("M-g l" . dumb-jump-quick-look))
  :init
  (setq dumb-jump-selector 'ivy
        dumb-jump-prefer-searcher 'rg))

(use-package highlight-symbol           ; Automatic and manual symbol highlighting for Emacs
  :bind (("C-c h s" . highlight-symbol)
         ("C-c h n" . highlight-symbol-next)
         ("C-c h p" . highlight-symbol-prev)
         ("C-c h r" . highlight-symbol-query-replace)))

(use-package yasnippet                  ; Snippets
  :commands (yas-reload-all yas-minor-mode)
  :hook ((prog-mode . yas-minor-mode))
  :config (yas-reload-all))

(use-package yasnippet-snippets         ; Official snippets collection
  :after yasnippet)

(use-package htmlize                    ; Convert buffer text/decorations into HTML
  :defer t)

(use-package company                    ; Text completion framework for Emacs
  :hook (prog-mode . company-mode)
  :config
  ;; Basic settings
  (setq company-dabbrev-ignore-case nil
        company-dabbrev-code-ignore-case nil
        company-dabbrev-downcase nil
        company-idle-delay 0.2
        company-tooltip-align-annotations t
        company-tooltip-flip-when-above t
        company-show-numbers t)

  ;; Add YASnippet support for all company backends
  ;; See: https://github.com/syl20bnr/spacemacs/pull/179
  (defvar company-mode-enable-yas t "Enable YASnippet for all company backends.")
  
  (defun ad|company-mode-backend-with-yas (backend)
    (if (or (not company-mode-enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
        backend
      (append (if (consp backend) backend (list backend))
              '(:with company-yasnippet))))

  (setq company-backends
        (mapcar #'ad|company-mode-backend-with-yas company-backends))

  ;; Turn off company support for certain modes
  (dolist (hook '(markdown-mode-hook org-mode-hook))
    (add-hook hook '(lambda () (company-mode -1)))))

(use-package company-statistics         ; Sort company completions SMRT
  :after company
  :config (company-statistics-mode 1))

(use-package company-quickhelp          ; Show popup documentation for company candidates
  ;; Disabled because the tooltip can't be styled on a Mac.
  ;; See: https://github.com/expez/company-quickhelp/issues/36
  :disabled t
  :after company
  :config (company-quickhelp-mode 1))

(use-package flycheck                   ; On the fly syntax checking for Emacs
  :config (global-flycheck-mode 1))

(use-package lsp-mode                   ; Language Server Protocol support for Emacs
  :commands lsp
  :hook (prog-mode . lsp-mode)
  :init
  ;; Prefer `lsp-ui' over flymake for errors
  (setq lsp-prefer-flymake nil)

  ;; Evil binding for `lsp-find-references'
  (evil-define-key '(normal visual) lsp-mode-map "gr" 'lsp-find-references)
  :config (require 'lsp-clients))

(use-package lsp-ui                     ; UI support for `lsp-mode'
  :hook (lsp-mode . lsp-ui-mode))

;; (use-package company-lsp
;;   :after (company lsp-mode)
;;   :config (add-to-list 'company-backends 'company-lsp)
;;   :custom
;;   (company-lsp-async t)
;;   (company-lsp-enable-snippet t))

;; Markdown support

(use-package markdown-mode              ; Major mode for editing Markdown/GFM files
  :commands (markdown-mode gfm-mode)
  :mode (("\\README\\.md\\'"  . gfm-mode)
         ("\\.md\\'"          . markdown-mode)
         ("\\.markdown\\'"    . markdown-mode))
  :init
  ;; Turn off auto-fill for GFM markdown
  (add-hook 'gfm-mode-hook #'turn-off-auto-fill)
  (add-hook 'gfm-mode-hook #'visual-line-mode)
  (setq markdown-command "multimarkdown")
  :config
  ;; Turn off 'M-q' in GFM markdown - it's too easy to do by mistake
  (bind-key "M-q" #'ignore gfm-mode-map))

(use-package markdown-preview-mode      ; Preview markdown
  :after markdown-mode
  :config
  (setq markdown-preview-stylesheets
        (list (concat "https://github.com/dmarcotte/github-markdown-preview/"
                      "blob/master/data/css/github.css"))))

;; Lisp/Emacs Lisp support

(use-package elisp-mode                 ; Major mode for editing Emacs Lisp files
  :ensure nil
  :bind (:map emacs-lisp-mode-map
              ("C-c m e r" . eval-region)
              ("C-c m e b" . eval-buffer)
              ("C-c m e e" . eval-last-sexp)
              ("C-c m e f" . eval-defun)))

;; Scala support

(use-package scala-mode                 ; Major mode for Scala
  :defer t
  :config
  ;; Indentation preferences
  (setq scala-indent:default-run-on-strategy
        scala-indent:operator-strategy)

  ;; Insert newline in a multiline comment should insert an asterisk
  (defun ad|scala-mode-newline-comments ()
    "Insert a leading asterisk in multiline comments, when hitting 'RET'."
    (interactive)
    (newline-and-indent)
    (scala-indent:insert-asterisk-on-multiline-comment))
  (define-key scala-mode-map (kbd "RET") #'ad|scala-mode-newline-comments))

(use-package sbt-mode                   ; Support for Satan's Build Tool
  :commands sbt-start sbt-command
  :config
  ;; Don't pop up SBT buffers automatically
  (setq sbt:display-command-buffer nil)

  ;; WORKAROUND: https://github.com/ensime/emacs-sbt-mode/issues/31
  ;; allows using SPACE when in the minibuffer
  (substitute-key-definition
   'minibuffer-complete-word
   'self-insert-command
   minibuffer-local-completion-map))

(use-package lsp-scala                  ; LSP support for Scala, using Metals
  :load-path "site-lisp/lsp-scala"
  :after (scala-mode lsp-mode)
  :demand t
  :hook (scala-mode . lsp)
  :init (setq lsp-scala-server-command (executable-find "metals-emacs")))

;; Python support

(use-package elpy                       ; Emacs Lisp Python environment
  :defer 4
  :init (setq elpy-rpc-backend "jedi"
              python-shell-interpreter "ipython"
              python-shell-interpreter-args "-i --simple-prompt"
              elpy-shell-echo-input nil
              python-check-command "/usr/local/bin/flake8")
  :config (elpy-enable))

(provide 'init)
;;; init.el ends here
