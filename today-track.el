;;; today-track.el --- -*- lexical-binding: t -*-

(require 'hydra)
(require 's)
(require 'dash)

(defvar today-track-types '("book" "project" "report" "course" "writing" "paper" "article")
  "Types of tracking entries.")

(defun today-track-rate ()
  "Set a 0-10 rating for the current entry."
  (interactive)
  (let ((ivy-sort-functions-alist nil)
        (rating-re (rx (1+ (or "★" "☆")))))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward rating-re (line-end-position) t 1)
          (let* ((rating-start (match-beginning 0))
                 (rating-end (match-end 0))
                 (rating (completing-read "rating: " (-map #'number-to-string (number-sequence 0 10)) nil t))
                 (rating (string-to-number rating)))
            (delete-region rating-start rating-end)
            (goto-char rating-start)
            (insert (s-repeat rating "★") (s-repeat (- 10 rating) "☆")))
        (message "no ratings found on this line.")))))

(defun today-track-get-dates-field ()
  "Return the bounds and dates for the dates-field on the current line."
  (let* ((date-pattern (rx
                        "["
                        (group-n 1
                                 (zero-or-one
                                  (repeat 4 digit) "-"
                                  (repeat 2 digit) "-"
                                  (repeat 2 digit)))
                        "]"
                        "--"
                        "["
                        (group-n 2
                                 (zero-or-one
                                  (repeat 4 digit) "-"
                                  (repeat 2 digit) "-"
                                  (repeat 2 digit)))
                        "]"
                        )))
    (save-excursion
      (beginning-of-line)
      (if (re-search-forward date-pattern (line-end-position) t 1)
          (let* ((dates (car (s-match-strings-all date-pattern (buffer-substring (line-beginning-position) (line-end-position)))))
                 (start-date (nth 1 dates))
                 (end-date (nth 2 dates)))
            (list (cons (match-beginning 0) (match-end 0)) start-date end-date))))))

(defun today-track-set-dates-field (bounds start-date end-date)
  "Set START-DATE and END-DATE for the dates-field in BOUNDS."
  (interactive)
  (save-excursion
    (delete-region (car bounds) (cdr bounds))
    (goto-char (car bounds))
    (insert (format "[%s]--[%s]" start-date end-date))))

(defun today-track-start ()
  "Set the started part of the dates field to today."
  (interactive)
  (let ((bounds-and-dates (today-track-get-dates-field))
        (start-date (format-time-string "%Y-%m-%d")))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (end-date (nth 2 bounds-and-dates)))
          (today-track-set-dates-field bounds start-date end-date)))))

(defun today-clear-started ()
  "Clear the started part of the dates field."
  (interactive)
  (let ((bounds-and-dates (today-track-get-dates-field)))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (end-date (nth 2 bounds-and-dates)))
          (today-track-set-dates-field bounds "" end-date)))))

(defun today-track-complete ()
  "Set the completed part of the dates field to today."
  (interactive)
  (let ((bounds-and-dates (today-track-get-dates-field))
        (end-date (format-time-string "%Y-%m-%d")))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (start-date (nth 1 bounds-and-dates)))
          (today-track-set-dates-field bounds start-date end-date)))))

(defun today-clear-completed ()
  "Clear the completed part of the dates field."
  (interactive)
  (let ((bounds-and-dates (today-track-get-dates-field)))
    (if (listp bounds-and-dates)
        (let ((bounds (nth 0 bounds-and-dates))
              (start-date (nth 1 bounds-and-dates)))
          (today-track-set-dates-field bounds start-date "")))))

(defun today-track-create-new ()
  ""
  (interactive)
  (let ((type (completing-read "type: " today-track-types)))
    (goto-char (point-min))
    (newline)
    (goto-char (point-min))

    (cond
     ((-contains-p '("book" "report" "paper" "article") type)
      (insert (format "* []--[] - ★☆ - %s - __ by  ()\n" type)))
     (t
      (insert (format "* []--[] - ★☆ - %s - __\n" type))))))

(defun today-track-create-new-from-entry-at-point ()
  "Create a new tracking-entry."
  (interactive)
  (let* ((heading (org-entry-get nil "ITEM"))
         (el (org-element-at-point))
         (beg (org-element-property :contents-begin el))
         (end (org-element-property :contents-end el))
         (content (when (and beg end) (buffer-substring beg end)))
         (type (completing-read "type: " today-track-types)))
    (org-cut-subtree)
    (goto-char (point-min))
    (newline)
    (goto-char (point-min))
    (insert (format "* []--[] - ★☆ - %s - %s\n%s" type heading (or content "")))))

(defhydra today-track-hydra (:foreign-keys run)
  ("s" (today-track-start) "start")
  ("S" (today-clear-started) "clear start")

  ("c" (today-track-complete) "complete")
  ("C" (today-clear-completed) "clear complete")

  ("n" (today-track-create-new) "create new entry")
  ("N" (today-track-create-new-from-entry-at-point) "create new entry from entry-at-point")

  ("r" (today-track-rate) "rate")

  ("q" nil "quit"))

(provide 'today-track)
