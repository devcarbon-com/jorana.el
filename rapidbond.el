;;; package --- Summary
;;; Commentary:
;;; Code:

(provide 'rapidbond)


(defun copy-current-file-link ()
  "Copy the link to the current file to the kill ring."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (kill-new (concat "file://" file-name))
      (message "Copied link to current file: %s" file-name))))

(defun copy-current-file-link-as-org-link ()
  "Copy the link to the current file as an org-mode link to the kill ring."
  (interactive)
  (let ((file-name (buffer-file-name)))
    (when file-name
      (kill-new (concat "[[file://" file-name "][" (file-name-nondirectory file-name) "]]"))
      (message "Copied link to current file as org-mode link: %s" file-name))))

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

(defun insert-id-tag ()
  "Return the current time in seconds from the epoch."
  (interactive)
  (insert (format "<<id:%s>>" (current-time-in-seconds))))

(defvar id-regex (concat "\\(#_\\|;\\|//\\|\\)\\( ?\\)\(?<<.*?>>\)?")
  "Regex that matches target tag comments.

   Example matches:
   #_<<tag>>
   #_(<<tag>>)
   ; <<tag>>
   //<<tag>>
   <<tag>>")

(defun hide-id-regex ()
  "Hide."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward id-regex nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
    (set-buffer-modified-p nil)))

(defun toggle-id-regex ()
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

(defun hide-id-regex ()
  "Hide."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward id-regex nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
    (set-buffer-modified-p nil)))
;; 
(defun create-org-link-to-line ()
  "Create an Org mode link to the contents of the line at point."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (line-number (line-number-at-pos))
         (line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (link-text (format "[[%s::%s][%s]]" file-name line-text line-text)))
    (kill-new link-text)))

(defun extract-target-from-line (line)
  "Extract the target from LINE using a regex that matches <<[anything]>> where [anything] is one or more characters. Return an Org mode link to the target with the absolute line number of the line containing the target."
  (let ((line-number (line-number-at-pos (point)))
        (beginning (save-excursion (goto-char (point-min)) (line-number-at-pos))))
    (if (string-match "<<\\(.+\\)>>" line)
        (let ((target (match-string 1 line)))
          (format "[[%s::%d][%s]]" (buffer-file-name) (+ line-number beginning) target))
      (format "Target not found on line %d" (+ line-number beginning)))))


(defun create-org-link-to-line-regex ()
  "Create an Org mode link to the target contained in the current line using a regex that matches <<id:[target]>> where target is an integer of any length."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (line-number (line-number-at-pos))
         (line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (target (if (string-match "<<\\(.+\\)>>" line-text)
                     (match-string 1 line-text)
                   line-number))
         (link-text (format "[[%s::%s][%s]]" file-name target target)))
    (kill-new link-text)
    (message link-text)))


(provide 'rapidbond)
;;; rapidbond.el ends here
