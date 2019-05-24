;;; today.el --- simple daily planning. -*- lexical-binding: t -*-

;; Copyright (C) 2018 Jens Christian Jensen

;; Author: Jens Christian Jensen <jensecj@gmail.com>
;; Keywords: org, org-mode, planning, today, todo
;; Package-Version: 20190412
;; Version: 0.9
;; Package-Requires: ((emacs "25.1") (org "9.0") (dash "2.14.1")  (s "1.12.0") (f "0.20.0") (hydra "0.14.0") (org-web-tools "0.1.0-pre"))

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

;; A simple tool to capture links to your org inbox.

;; A collection of commands that enable a certain workflow of creating,
;; manipulating, and ordering files, to enable low-hassle daily planning.

;;; Notes on structure:

;; Entries are collected in a central file, `today-file', and when completed
;; each entry can be archived as a file in a directory inside of
;; `today-directory', whose name is the date for that entry. The org file for
;; the entry resides in this directory, and is also named with the date, ending
;; with the `.org' extension.  This is done so that each entry can have its own
;; extra content (e.g. images, pdfs), reside in its directory and not interfere
;; with the other entries.

;;; Code:

(require 'hydra)
(require 'today-capture)
(require 'today-archive)
(require 'today-track)

(defcustom today-directory
  (concat user-emacs-directory "today-planner")
  "Directory used for planning files.")

(defcustom today-file
  (concat today-directory "today.org")
  "Things to do today.")

(defcustom today-inbox-file
  (concat today-directory "inbox.org")
  "Accumulating file for today entries.")

(defun today--path-from-date (date)
  "Returns the path to the file corresponding to DATE."
  (f-join today-directory date (concat date ".org")))

(defun today--visit-date-file (date)
  "Visit the file for DATE, create it if it does not exist."
  (let ((date-file (today--path-from-date date)))
    (find-file date-file)))

;;;###autoload
(defun today ()
  "Visit the today file, containing entries."
  (interactive)
  (find-file today-file))

;;;###autoload
(defun today-visit-todays-file ()
  "Visit today's file, create it if it does not exist."
  (interactive)
  (xref-push-marker-stack)
  (today--visit-date-file (format-time-string "%Y-%m-%d")))

;;;###autoload
(defun today-list ()
  "List all files from `today-directory', visit the one
selected."
  (interactive)
  (let* ((ivy-sort-functions-alist nil) ;; dates are already sorted
         (dates (reverse (-map #'f-base (f-directories today-directory))))
         (date (completing-read "Date: " dates)))
    (xref-push-marker-stack)
    (today--visit-date-file date)))

(provide 'today)
