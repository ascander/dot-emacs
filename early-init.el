;;; early-init.el --- Early initialization       -*- lexical-binding: t; -*-

;; Copyright (C) 2020  Ascander Dost

;; Author: Ascander Dost
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

;;

;;; Code:

;; Package initialization happens before loading `user-init-file' but after
;; `early-init-file'. Since we handle package initialization explicitly
;; ourselves, we don't need Emacs to do it for us.
(setq package-enable-at-startup nil)

;; Disable tool bar, scroll bar, and menu bar BEFORE the first frame is created.
;; Note: the menu bar can't actually be disabled on MacOS, so we check for that.
(unless (and (display-graphic-p) (eq system-type 'darwin))
  (push '(menu-bar-lines . 0) default-frame-alist))
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

;; Set font early
(push '(font . "Iosevka Dost") default-frame-alist)

(provide 'early-init)
;;; early-init.el ends here
