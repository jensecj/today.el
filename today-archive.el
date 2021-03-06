;;; today-archive.el --- -*- lexical-binding: t -*-

(require 'f)
(require 's)
(require 'dash)
(require 'org-web-tools)
(require 'cl-lib)
(require 'org-ql)

(defun today-move-to-today ()
  "Move an entry to the today file."
  (interactive)
  (when (org-at-heading-p)
    (let ((file today-file)
          (org-yank-adjusted-subtrees t))
      (org-cut-subtree)
      (save-buffer)

      ;; TODO: dont use `find-file-noselect'
      (with-current-buffer (find-file-noselect file)
        (goto-char (point-min))
        (org-yank)
        (goto-char (point-min))
        (save-buffer)))))

(defun today-refile (file headline &optional arg)
  (let ((pos (save-excursion
               (find-file file)
               (org-find-exact-headline-in-buffer headline))))
    (org-refile arg nil (list headline file nil pos)))
  (switch-to-buffer (current-buffer)))

(defun today-archive--date-file (date)
  "Return filepath of todays archive file."
  (let* ((date-file (f-join today-directory date (concat date ".org"))))
    date-file))

(defun today-archive-done-todos ()
  "Archive all completed TODOs in the current file."
  (interactive)
  (org-map-entries
   (lambda ()
     (let* ((org-archive-file-header-format "")
            (org-archive-save-context-info '(time file olpath itags))

            (closed-date (assoc "CLOSED" (org-entry-properties)))
            (date-format (when (cdr closed-date) (substring (cdr closed-date) 1 11)))
            (date-file (today-archive--date-file (format-time-string
                                                  (if date-format date-format "%Y-%m-%d"))))
            (dir (f-dirname date-file))
            (org-archive-location (concat date-file "::")))

       (unless (f-exists-p dir)
         (f-mkdir dir))

       (unless (f-exists-p date-file)
         (f-touch date-file))

       (org-archive-subtree)

       ;; TODO: dont use `find-file-noselect'
       (with-current-buffer (find-file-noselect date-file)
         (when (buffer-modified-p)
           (save-buffer)))

       (setq org-map-continue-from (outline-previous-heading))))
   "/DONE" 'file))


(provide 'today-archive)
