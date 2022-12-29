;;; package --- Summary
;;; Commentary:
;;; Code:

(provide 'jorana)

(defun copy-current-file-link-as-org-transclusion-link ()
  "Copy the org-transclusion link to the file visited by the most recently used buffer, and also yank the most recent item from the kill-ring into the current buffer."
  (interactive)
  (with-current-buffer (other-buffer (current-buffer) t)
    (let ((file-name (buffer-file-name)))
      (when file-name
        (kill-new (concat "#+transclude: [[file://" file-name "][" (file-name-nondirectory file-name) "]]"))
        (message "Copied link to file as org-mode link: %s" file-name))))
  (yank))

(defun current-time-in-seconds ()
  "Return the current time in seconds from the epoch."
  (format-time-string "%s" (current-time)))

(defun gen-id-tag ()
  (format "<id:%s>" (current-time-in-seconds)))

(defun insert-id-tag () ;<id:1672227875>
  "Return the current time in seconds from the epoch."
  (interactive)
  (insert (gen-id-tag)))

(defun with-buffer-visiting (file func)
  "Call FUNC with the buffer visiting FILE as the current buffer."
  (let ((buffer (find-buffer-visiting file)))
    (if buffer
        (with-current-buffer buffer
          (funcall func))
      (error "No buffer visiting file %s" file))))

(defun get-line-contents (line)
  "Returns the line contents for LINE, with leading and trailing whitespace trimmed."
  (let ((start (line-beginning-position line))
        (end (line-end-position line)))
    (string-trim (buffer-substring start end))))

(defun remove-non-symbol-chars (string)
  "Replace all non-symbol characters in STRING with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" string))

(defun insert-file-link () ;<id:1672242704>
  (interactive)
  (insert (plist-get (find-file-line-link) :link)))

(defvar id-regex (concat "\\(#_\\|;\\|//\\|\\)\\( ?\\)\(?<id:.*?>\)?")
  "Regex that matches target tag comments.

   Example matches:
   #_<id:tag>
   #_(<id:tag>)
   ; <id:tag>
   //<id:tag>
   <id:tag>")

(defun hide-id-regex () ;<id:1672243157>
  "Hide."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward id-regex nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
    (set-buffer-modified-p nil)))



(defun toggle-id-regex () ;
  "Toggle the visibility of a regex that matches `#_(id:<number>)`."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward id-regex nil t)
      (let ((invisible (get-text-property (match-beginning 0) 'invisible)))
        (if invisible
            (remove-text-properties (match-beginning 0) (match-end 0) '(invisible t))
          (add-text-properties (match-beginning 0) (match-end 0) '(invisible t))))
      (set-buffer-modified-p nil))))

(defun hide-id-regex () ;<id:1672242977>
  "Hide."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward id-regex nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
    (set-buffer-modified-p nil)))

(defun create-org-link-to-line ()
  "Create an Org mode link to the contents of the line at point."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (line-number (line-number-at-pos))
         (line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (link-text (format "[[%s::%s][%s]]" file-name line-text line-text)))
    (kill-new link-text)))

(defun marker-at-line (line) ;<id:1672242453>
  "Create a marker at the beginning of LINE in the current buffer."
  (set-marker (make-marker) (line-beginning-position line)))

(defun marker-at-point (point &rest buffer) ;<id:1672242453>
  "Create a marker at POINT in BUFFER."
  (set-marker (make-marker) point (car (or buffer (current-buffer)))))
;;

(defun append-to-line (line string)
  "Append STRING to the end of LINE in the current buffer."
  (end-of-line)
  (insert string))

(defun extract-target-from-line (line &rest generate-when-missing comment-string)
  "Extract the target from LINE using a regex that matches <<[anything]>>.
where [anything] is one or more characters. Return an Org mode link to the target."
  (let* ((line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (target (when (string-match "\\(<.*>\\)" line-text)
                   (match-string 1 line-text))))
    (cond ((stringp target) (substring-no-properties target))
          (generate-when-missing
           (let ((target (gen-id-tag)))
             (append-to-line line (concat " ;" target))
             target))
          (:no-target-found line-number))))

(defun target-at-point ()
  (interactive)
  (extract-target-from-line (line-number-at-pos (point)) t)
  )

(defun current-non-hidden-buffers ()
  "Return a list of the current non-hidden buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (seq-filter (lambda (buffer) (not (string-prefix-p " " (buffer-name buffer)))) buffers)))

(defun search-target-in-last-used-buffers* (target bullseye buffers)
  "Search for the contents of TARGET at point in the last 5 used buffers.
Jump to the first occurrence if found. BUFFERS is a list of buffers to search."
  (if target
      (if buffers
          (let ((buffer (car buffers)))
            (let ((marker (save-excursion
                            (set-buffer buffer)
                            (beginning-of-buffer)
                            (if (search-forward target nil t)
                                (marker-at-point (match-beginning 0) (current-buffer))
                              (search-target-in-last-used-buffers* target
                                                                   bullseye
                                                                   (cdr buffers))))))
              (when marker
                (switch-to-buffer (marker-buffer marker))
                (goto-char marker)
                (beginning-of-line)
                (forward-char bullseye))))
        (message "Target not found in any of the last 5 used buffers."))
    (message "No target at point to search for.")))

(defun search-target-in-last-used-buffers ()
  (interactive)
  (search-target-in-last-used-buffers*
   (string-trim (substring-no-properties (thing-at-point 'line)))
   (current-column)
   (cl-subseq (current-non-hidden-buffers) 1 5)))
search-target-in-last-used-buffers

search-target-in-last-used-buffers



(defun find-file-line-link () ;<id:1672243830>
  "Prompt the user to select a file using completion.
Return a link to the file in the current buffer using
a relative path from the current project root.

The returned plist contains the following keys:

:link - the 'org-mode' link string.
:file - the relative path to the file from the current project.
:target - the target of the link. This is an id like '<id:1172243297>'
          so that links still work after refactoring."
  (save-excursion
    (let* ((root (projectile-project-root))
           (file (read-file-name "Select file: " nil file-name-history))
           (relative-file (concat "file:" (file-relative-name file root)))
           (link (let ((buffer (find-buffer-visiting file)))
                   (with-current-buffer buffer
                     (let* ((marked (consult-line))
                            (line (line-number-at-pos (point-marker)))
                            (line-point (bounds-of-thing-at-point 'line)))
                       (list :line line
                             :target (extract-target-from-line line t)
                             :text (remove-non-symbol-chars (buffer-substring (car line-point) (- (cdr line-point) 1))))))))
           (line (plist-get link :line))
           (line-string (plist-get link :text))
           (target (plist-get link :target)))
      (list :link (format "[[%s::%s][%s]]" relative-file target line-string)
            :file file
            :target target))))

(defun find-and-insert-transclusion () ;<id:1672243297>
  "Find file and thing to transclude into current buffer."
  (interactive)
  (let* ((line-link (find-file-line-link))
         (link (plist-get line-link :link))
         (file (plist-get line-link :file))
         (lang (let ((buffer (find-buffer-visiting file)))
                 (with-current-buffer buffer
                   (downcase (car mode-name))))))
    (insert (format "#+transclude: %s :src %s :thing-at-point sexp" link lang))
    (org-transclusion-add)))


(provide 'jorana)
;;; jorana.el ends here
