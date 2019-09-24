;;; init-org.el --- Org mode configuration           -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Ascander Dost

;; Author: Ascander Dost <dostinthemachine@gmail.com>
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This file contains `org-mode' settings.

;;; Code:

(use-package org                        ; The almighty Org mode
  :ensure org-plus-contrib
  :pin org
  :mode (("\\.org$" . org-mode))
  :general (general-spc
             "o"  #'(:ignore t :which-key "Org")
             "oa" #'org-agenda
             "oc" #'org-capture
             "oj" #'counsel-org-goto
             "ol" #'org-store-link)
  :config
  ;; Defaults
  (setq
   org-src-fontify-natively t           ; fontify text in code blocks
   org-log-done 'time                   ; timestamp done states
   org-use-fast-todo-selection t        ; fast-select TODO states
   org-startup-truncated nil            ; don't start up truncated
   org-tags-column 80                   ; indent tags to here
   org-enable-priority-commands nil     ; disable priority commands
   org-reverse-note-order t)            ; store new notes at the beginning

  ;; Force org popup buffers to follow Shackle rules
  ;; See: https://emacs.stackexchange.com/a/31634
  (setq org-src-window-setup 'other-window)

  ;; Re-define org-switch-to-buffer-other-window to NOT use org-no-popups.
  ;; Primarily for compatibility with shackle.
  (defun org-switch-to-buffer-other-window (args)
    "Switch to buffer in a second window on the current frame.
In particular, do not allow pop-up frames. Returns the newly
created buffer. Redefined to allow pop-up windows."
    ;;  (org-no-popups
    ;;     (apply 'switch-to-buffer-other-window args)))
    (switch-to-buffer-other-window args))

  ;; Export backends
  (setq org-export-backends '(md odt latex icalendar html ascii))

  ;; Enable easy templates for code blocks, etc.
  ;; See discussion at: https://lists.gnu.org/archive/html/emacs-orgmode/2018-04/msg00600.html
  (require 'org-tempo)

  ;; Enable org habits for habit tracking
  (require 'org-habit)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Tasks & States
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; TODO task states
  (setq org-todo-keywords
        '((sequence "TODO(t)" "NEXT(n)" "|" "DONE(d)")
          (sequence "WAITING(w@/!)" "HOLD(h@/!)" "|" "CANCELLED(c@/!)" "MEETING")))

  ;; Don't allow changes of TODO state unless it makes sense
  (setq org-enforce-todo-dependencies t
        org-enforce-todo-checkbox-dependencies t)

  ;; Allow changing state with 'shift+arrow' without logging notes or timestamps
  (setq org-treat-S-cursor-todo-selection-as-state-change nil)

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
  (setq org-todo-state-tags-triggers
        '(("CANCELLED" ("CANCELLED" . t))
          ("WAITING" ("WAITING" . t))
          ("HOLD" ("WAITING" . t) ("HOLD" . t))
          (done ("WAITING") ("HOLD"))
          ("TODO" ("WAITING") ("CANCELLED") ("HOLD"))
          ("NEXT" ("WAITING") ("CANCELLED") ("HOLD"))
          ("DONE" ("WAITING") ("CANCELLED") ("HOLD"))))

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org Capture Settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Capture templates
  (setq org-capture-templates
        '(("t" "Todo" entry (file "~/org/refile.org")
           "* TODO %i%?")
          ("n" "Note" entry (file "~/org/refile.org")
           "* %i%? :NOTE:\n %U")
          ("m" "Meeting" entry (file "~/org/refile.org")
           "* MEETING with %? :MEETING:\n %U")
          ("d" "Deadline" entry (file "~/org/reminders.org")
           "* TODO %i%?\n DEADLINE:%T")))

  ;; Refile targets include this file and any agenda file - up to 5 levels deep
  (setq org-refile-targets '((nil :maxlevel . 5)
                             (org-agenda-files :maxlevel . 5)))

  ;; Switch to insert state when capturing
  (add-hook 'org-capture-mode-hook #'evil-insert-state)
  (add-hook 'org-log-buffer-setup-hook #'evil-insert-state)

  ;; Include the filename in refile target paths; this allows refiling to the
  ;; top level of a target
  (setq org-refile-use-outline-path 'file)

  ;; Allow refiling to create parent tasks with confirmation
  (setq org-refile-allow-creating-parent-nodes 'confirm)

  ;; Do not use hierarchical steps in completion, since we use Ivy
  (setq org-outline-path-complete-in-steps nil)

  ;; Exclude tasks in done states from being refile targets; taken from:
  ;; http://doc.norang.ca/org-mode.html#RefileSetup
  (defun ad|validate-refile-target ()
    "Exclude todo keywords with a done state from refile targets."
    (not (member (nth 2 (org-heading-components)) org-done-keywords)))

  (setq org-refile-target-verify-function 'ad|validate-refile-target)

  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;; Org Agenda Settings
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Display agenda as the only window, but restore the previous configuration
  ;; afterwards; let's be polite.
  (setq org-agenda-window-setup 'only-window
        org-agenda-restore-windows-after-quit t)

  ;; Do not dim blocked tasks
  (setq org-agenda-dim-blocked-tasks nil)

  ;; Compact agenda blocks (disabled)
  (setq org-agenda-compact-blocks nil)

  ;; Agenda files
  (setq org-agenda-files '("~/org/work.org"
                           "~/org/home.org"
                           "~/org/refile.org"
                           "~/org/reminders.org"
                           "~/org/emacs.org"))

  ;; === Utility functions for agenda view ===
  (defun ad|is-project-p ()
    "Is this a task with a todo keyword subtask?"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task has-subtask))))

  (defun ad|skip-non-projects ()
    "Skip trees that are not projects."
    (if (save-excursion (ad|skip-non-stuck-projects))
        (save-restriction
          (widen)
          (let ((subtree-end (save-excursion (org-end-of-subtree t))))
            (cond
             ((ad|is-project-p)
              nil)
             ((and (ad|is-project-subtree-p) (not (ad|is-task-p)))
              nil)
             (t
              subtree-end))))
      (save-excursion (org-end-of-subtree t))))

  (defun ad|is-task-p ()
    "Any task with a todo keyword and no subtask"
    (save-restriction
      (widen)
      (let ((has-subtask)
            (subtree-end (save-excursion (org-end-of-subtree t)))
            (is-a-task (member (nth 2 (org-heading-components)) org-todo-keywords-1)))
        (save-excursion
          (forward-line 1)
          (while (and (not has-subtask)
                      (< (point) subtree-end)
                      (re-search-forward "^\*+ " subtree-end t))
            (when (member (org-get-todo-state) org-todo-keywords-1)
              (setq has-subtask t))))
        (and is-a-task (not has-subtask)))))

  (defun ad|is-project-subtree-p ()
    "Any task with a todo keyword that is in a project subtree.
Callers of this function already widen the buffer view."
    (let ((task (save-excursion (org-back-to-heading 'invisible-ok)
                                (point))))
      (save-excursion
        (ad|find-project-task)
        (if (equal (point) task)
            nil
          t))))

  (defun ad|skip-non-stuck-projects ()
    "Skip trees that are not stuck projects."
    (save-restriction
      (widen)
      (let ((next-headline (save-excursion (or (outline-next-heading) (point-max)))))
        (if (ad|is-project-p)
            (let* ((subtree-end (save-excursion (org-end-of-subtree t)))
                   (has-next ))
              (save-excursion
                (forward-line 1)
                (while (and (not has-next) (< (point) subtree-end) (re-search-forward "^\\*+ NEXT " subtree-end t))
                  (unless (member "WAITING" (org-get-tags-at))
                    (setq has-next t))))
              (if has-next
                  next-headline
                nil))                   ; a stuck project has subtasks, but no 'NEXT' task
          next-headline))))

  (defun ad|find-project-task ()
    "Move point to the parent (project) task if any"
    (save-restriction
      (widen)
      (let ((parent-task (save-excursion (org-back-to-heading 'invisible-ok) (point))))
        (while (org-up-heading-safe)
          (when (member (nth 2 (org-heading-components)) org-todo-keywords-1)
            (setq parent-task (point))))
        (goto-char parent-task)
        parent-task)))

  (defun ad|skip-project-tasks ()
    "Show non-project tasks.
Skip project and sub-project tasks, habits, and project related tasks."
    (save-restriction
      (widen)
      (let* ((subtree-end (save-excursion (org-end-of-subtree t))))
        (cond
         ((ad|is-project-p)
          subtree-end)
         ((ad|is-project-subtree-p)
          subtree-end)
         (t
          nil)))))

  (defun ad|org-auto-exclude-function (tag)
    "Automatic task exclusion in the agenda with '/ RET'."
    (and (cond
          ((string= tag "hold") t)
          ((string= tag "@home") t)
          ((string= tag "emacs") t))
         (concat "-" tag)))

  (setq org-agenda-auto-exclude-function 'ad|org-auto-exclude-function)

  ;; Custom agenda commands
  (setq org-agenda-custom-commands
        '(("N" "Notes" tags "NOTE"
           ((org-agenda-overriding-header "Notes")
            (org-tags-match-list-sublevels t)))
          (" " "Agenda" ((agenda "" nil)
                         (tags-todo "-CANCELLED/!"
                                    ((org-agenda-overriding-header "Stuck Projects:")
                                     (org-agenda-skip-function 'ad|skip-non-stuck-projects)
                                     (org-agenda-sorting-strategy '(category-keep))))
                         (tags-todo "-HOLD-CANCELLED/!"
                                    ((org-agenda-overriding-header "Projects:")
                                     (org-agenda-skip-function 'ad|skip-non-projects)
                                     (org-agenda-sorting-strategy '(category-keep))))
                         (tags-todo "-HOLD-CANCELLED-ARCHIVE/!NEXT"
                                    ((org-agenda-overriding-header "Next Tasks:")))
                         (tags-todo "-REFILE-CANCELLED-WAITING-HOLD-REMINDER-HABIT/!"
                                    ((org-agenda-overriding-header "Charlie Work:")
                                     (org-agenda-skip-function 'ad|skip-project-tasks)
                                     (org-agenda-sorting-strategy '(category-keep))))
                         (tags "REFILE"
                               ((org-agenda-overriding-header "Refile:")
                                (org-tags-match-list-sublevels nil)))))))

  ;; Archiving
  (setq org-archive-location "~/org/archive.org::* From %s")

  ;; Navigate by headings, using Ivy
  (setq org-goto-interface 'outline-path-completion
        org-outline-path-complete-in-steps nil))

(use-package org-bullets                ; Fancy utf-8 bullets for org mode
  :hook ((org-mode . org-bullets-mode)))

(use-package org-cliplink               ;   insert links from the clipboard
  :bind ("C-c L" . org-cliplink))

(provide 'init-org)
;;; init-org.el ends here

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:
