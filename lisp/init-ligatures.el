;;; init-ligatures.el --- Ligatures for use with Iosevka  -*- lexical-binding: t; -*-

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
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Font ligatures setup

;;; Code:

;; Show non-prettified when point is on, or immediately after, the symbol.
(setq prettify-symbols-unprettify-at-point 'right-edge)

(defun ad|setup-iosevka-ligatures ()
  "Append Iosevka ligatures in PUA to `prettify-symbols-alist'."
  (setq prettify-symbols-alist
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
                  ("<!---" . ?)
                  ))))

(defun ad|refresh-pretty ()
  "Explicitly toggle Prettify-Symbols mode off and back on."
  (prettify-symbols-mode -1)
  (prettify-symbols-mode +1))

;; Hooks for modes in which to install the Iosevka ligatures
(mapc (lambda (hook)
        (add-hook hook (lambda () (ad|setup-iosevka-ligatures) (ad|refresh-pretty))))
      '(text-mode-hook
        prog-mode-hook))
(global-prettify-symbols-mode +1)

(provide 'init-ligatures)
;;; init-ligatures.el ends here
