;;; today-capture.el ---  -*- lexical-binding: t -*-

(require 'f)
(require 's)
(require 'dash)
(require 'org-web-tools)
(require 'cl-lib)
(require 'async)

(defvar today-capture-ytdl-path (executable-find "youtube-dl")
  "path to the `youtube-dl' binary.")

(defun today-capture--url-to-org-link (url)
  "Try to get the website title of URL, then convert into
`org-link' format."
  (let ((title (today-capture--title-from-url url)))
    (org-make-link-string url title)))

(defun today-capture--title-from-url (url)
  "Get the website title of URL."
  (let* ((html (org-web-tools--get-url url))
         (title (org-web-tools--html-title html))
         (title (s-replace "[" "(" title))
         (title (s-replace "]" ")" title)))
    title))

(defun today-capture--count-lines-from-url (url)
  "Get the number of lines on the website of URL.

Requires system tool `lynx'."
  (let ((lines (s-trim (shell-command-to-string (format "lynx -dump %s | wc -l" url)))))
    (when (< (length lines) 5) lines)))

(defun today-capture--date-from-wayback-machine (url)
  "Try to return the date of the first time the wayback machine
saw URL. This may not be the real publishing date for URL."
  (let* ((wa-query "http://web.archive.org/cdx/search/cdx?url=")
         (wa-params "&fl=timestamp&output=json&limit=1")
         (query (s-concat wa-query url wa-params))
         (response (->> (shell-command-to-string (format "curl -s \"%s\"" query)) (s-trim)))
         (jq "jq -r '.[1]? | add?'")
         (date (unless (s-blank-str-p response)
                 (->> (shell-command-to-string (format "echo '%s' | %s" response jq)) (s-trim)))))
    (when (not (or (s-blank-str? date)
                   (string= (substring date 0 11) "parse error")))
      (let ((year (substring date 0 4))
            (month (substring date 4 6))
            (day (substring date 6 8)))
        (format "%s-%s-%s" year month day)))))

(defun today-capture-fix-youtube-org-link-at-point ()
  "Add upload-date and duration to a youtube org-link."
  (interactive)
  (when-let* ((url (thing-at-point 'url))
              (bounds (bounds-of-thing-at-point 'url))
              (content (today-capture--youtube-video url)))
    (delete-region (car bounds) (cdr bounds))
    (goto-char (car bounds))
    (insert content)))

(defun today-capture--youtube-get-upload-date (url)
  "Return the upload date for a youtube URL.
Requires system tools `youtube-dl' and `jq'."
  (let* ((ytdl "--simulate --dump-json --no-warnings")
         (jq "jq '.upload_date'")
         (raw-date (shell-command-to-string
                    (format "%s %s '%s' | %s"
                            today-capture-ytdl-path ytdl url jq)))
         (clean-date (s-replace "\"" "" (s-trim raw-date)))
         (year (substring clean-date 0 4))
         (month (substring clean-date 4 6))
         (day (substring clean-date 6 8))
         (date (format "%s-%s-%s" year month day)))
    (when (and year month day)
      date)))

(defun today-capture--youtube-duration-from-url (url)
  "Get the duration of a youtube video.

Requires system tool `youtube-dl'."
  (let* ((args "--get-duration --no-warnings")
         (raw-duration (shell-command-to-string
                        (format "%s %s '%s'"
                                today-capture-ytdl-path
                                args url))))
    (when (<= (length raw-duration) 10) ;HACK: if its longer, its probably an error
      (s-trim raw-duration))))

(defun today-capture--youtube-playlist (url)
  "Capture a youtube playlist from URL."
  (let* ((base-cmd (format "%s %s" today-capture-ytdl-path "--no-warnings"))
         (playlist-entries-query (format "%s --flat-playlist '%s' -j | jq '.title, .url'" base-cmd url))
         (playlist-title-query (format "%s --playlist-end 1 -j '%s' | jq '.playlist'" base-cmd url))
         (entries (shell-command-to-string playlist-entries-query))
         (entries (s-replace "\"" "" entries))
         (entries (s-split "\n" entries))
         (entries (-partition 2 entries))
         (title (shell-command-to-string playlist-title-query))
         (final))

    (-each entries
      (lambda (e)
        (when (cadr e)
          (push
           (format "[[https://www.youtube.com/watch?v=%s][%s]]" (cadr e) (car e))
           final))))

    (with-temp-buffer
      (newline)
      (insert "* " (format "[[%s][%s]]" url title))
      (newline)

      (-map
       (lambda (e) (insert "** " e) (newline))
       (reverse final))

      (buffer-string))))

(defun today-capture--youtube-video (url)
  "Capture a youtube video from URL."
  (let* ((title (today-capture--title-from-url url))
         (title (replace-regexp-in-string " - YouTube$" "" title))
         (title (replace-regexp-in-string " - Invidious$" "" title))
         (duration (today-capture--youtube-duration-from-url url))
         (upload-date (today-capture--youtube-get-upload-date url))
         (org-link (org-make-link-string url title)))
    (cons org-link `(("DATE" . ,upload-date) ("DURATION" . ,duration)))))

(defun today-capture--read-link-handler (link)
  "Handler for READ task. Expects CONTENT to be a link to some
website. Will try to extract the number of lines on the website,
and add to the front of the entry. Will also try to extract the
title of the website, and convert the link into an `org-mode'
link, using the title."
  (let* ((date (today-capture--date-from-wayback-machine link))
         (lines (today-capture--count-lines-from-url link))
         (org-link (today-capture--url-to-org-link link)))
    (cons org-link `(("DATE" . ,date) ("LOC" . ,lines)))))

(defun today-capture--watch-link-handler (link)
  "Handler for the WATCH task. Expects the LINK to be a source
compatible with `youtube-dl'. Will try to extract the title of the
link, and create an `org-mode' link using that title, will also
extract the duration of the video."
  (if (string-match-p (regexp-quote ".com/playlist?list=") link)
      (today-capture--youtube-playlist link)
    (today-capture--youtube-video link)))

(defun today-capture--plain-link-handler (link)
  "Handler for the PLAIN task. Will try to extract the title of the
link, and create an `org-mode' link using that title."
  (let ((org-link (today-capture--url-to-org-link link)))
    ;; (format "%s" org-link)
    (cons org-link nil)))

(defvar today-capture-handlers-alist
  '((read . today-capture--read-link-handler)
    (watch . today-capture--watch-link-handler)
    (plain . today-capture--plain-link-handler))
  "List of capture tasks and their associated handlers. A handler
  recieves the capture content as a parameter.")

(defun today-capture--apply-handler (task entry)
  "If a handler exists for TASK, then return the result of
applying handler on ENTRY, otherwise return ENTRY."
  (let ((handler (assoc task today-capture-handlers-alist)))
    (if handler
        (funcall (cdr handler) entry)
      entry)))

;;;###autoload
(defun today-capture-async (task entry buffer &optional level)
  "Captures an ENTRY with TASK, into BUFFER, asynchronously."
  (async-start
   `(lambda ()
      ,(async-inject-variables "^load-path$")
      (require 'today)

      (today-capture--apply-handler ',task ,entry))
   `(lambda (result)
      ,(async-inject-variables "^load-path$")
      (require 'today)

      (let* ((datetime (format-time-string "%Y-%m-%d %H:%M:%S"))
             (headline (car result))
             (props (cdr result))
             (subtree (format "* %s" headline)))
        (with-current-buffer ,buffer
          (save-excursion
            (goto-char (point-max))
            (org-paste-subtree ,level subtree)
            (org-set-property "CAPTURED_TIME" datetime)
            (dolist (p props)
              (when-let* ((prop (car p))
                          (val (cdr p)))
                (org-set-property prop val)))
            (org-cycle)                 ;org-set-property unfolds the subtrees properties, refold them.
            (save-buffer)))))))

;;;###autoload
(defun today-capture (task entry &optional buffer)
  "Capture ENTRY with TASK into todays file."
  (today-capture-async task entry (or buffer (find-file-noselect today-inbox-file)) 2))

(defun today-capture-link-with-task-from-clipboard (task)
  "Capture ENTRY with TASK into todays file."
  (let* ((entry (substring-no-properties
                 (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
    (message "capturing from clipboard: %s" entry)
    (today-capture task entry)))

;;;###autoload
(defun today-capture-link-with-task (task)
  "Prompt for ENTRY, then capture with TASK into today's file."
  (letrec ((link (completing-read "link: " '())))
    (today-capture task link)))

;;;###autoload
(defun today-capture-prompt ()
  "Captures a LINK into today's file, with the selected TASK."
  (interactive)
  (let ((task (intern (completing-read "task: " today-capture-handlers-alist)))
        (entry (completing-read "entry: " '())))
    (today-capture task entry)))

(defun today-capture-here-from-clipboard ()
  "Capture url from clipboard to current buffer."
  (interactive)
  (let ((task (intern (completing-read "task: " today-capture-handlers-alist)))
        (entry (substring-no-properties
                (gui-get-selection 'CLIPBOARD 'UTF8_STRING))))
    (today-capture-async task entry (current-buffer))))

;;;###autoload
(defun today-capture-elfeed-at-point ()
  "Captures a TASK from selected elfeed entry."
  (interactive)
  (let* ((entry (car (elfeed-search-selected)))
           (link (elfeed-entry-link entry))
           (title (elfeed-entry-title entry))
           (org-link (org-make-link-string link title)))
    (elfeed-untag entry 'unread)
    (elfeed-search-update-entry entry)
    (today-capture 'elfeed (cons org-link nil))
    (next-line)))

(provide 'today-capture)
