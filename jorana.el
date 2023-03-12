;;; package --- Summary
;;; Package-Requires: ((names "") (transient "") (consult "") (projectile "") (emacs "24"))



;;; Commentary:
;;; Code:

(provide 'jorana)
(eval-when-compile (require 'names))


(use-package transient)
(define-namespace jorana-

(defvar current-narrative nil)
(defvar thing-to-use "sexp")

(defun get-or-create-buffer-for-file (filepath)
  "Return the buffer for FILEPATH, or open buffer if it does not yet exist.
We don't just use 'find-file-noselect because it would not include unsaved changes."
  (or (find-buffer-visiting filepath)
      (find-file-noselect filepath)))

(defun current-time-in-seconds ()
  "Return the current time in seconds from the epoch."
  (format-time-string "%s" (current-time)))

(defun gen-id-tag ()
  (format "<id:%s>" (current-time-in-seconds)))

(defun insert-id-tag () ;<id:1672227875>
  "Insert a jorana id. Currently, the current time in seconds from the epoch."
  (interactive)
  (insert (gen-id-tag)))

(defun get-line-contents (line)
  "Return the line contents for LINE, with leading and trailing whitespace trimmed."
  (let ((start (line-beginning-position line))
        (end (line-end-position line)))
    (string-trim (buffer-substring start end))))

(defun remove-non-symbol-chars (string)
  "Replace all non link-safe characters in STRING with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" string))

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

(defun toggle-id-visibility () ;
  "Toggle the visibility id markers IE. `#_(id:<number>)`."
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

(defun target-at-point ()
  (interactive)
  (extract-target-from-line (line-number-at-pos (point)) t)
  )

(defun current-non-hidden-buffers ()
  "Return a list of the current non-hidden buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (seq-filter (lambda (buffer) (not (string-prefix-p " " (buffer-name buffer)))) buffers)))

(defun search-target-in-last-used-buffers* (target bullseye buffers) ;<id:1672282124>
  "Search for the contents of TARGET at point in the last 5 used buffers.
Jump to the first occurrence if found. BUFFERS is a list of buffers to search."
  ;; TODO this would be much more reliable if if were to instead of searching use the precise offset from the transclution start.
  ;; UPDATE turns out the text properties have this information. 
  ;; example:  (get-text-property (point) 'tc-src-beg-mkr)
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

(defun search-target-in-last-used-buffers () ;<id:1672282092>
  (interactive)
  (search-target-in-last-used-buffers*
   (string-trim (substring-no-properties (thing-at-point 'line)))
   
   (current-column)
   (cl-subseq (current-non-hidden-buffers) 1 5)))

(defun target-from-line (line) ;<id:1678573021>
  "Target from LINE."
  (let ((on-org-heading (and (eq major-mode 'org-mode)
                             (org-on-heading-p))))
    (if on-org-heading
        (org-entry-put nil "CUSTOM_ID" (org-id-get-create))
      ;; get id from line if it already exists
      (prog2 (string-match "\\(<.*>\\)" line-text)
          (match-string 1 line-text)))))

(defun extract-target-from-line! (line &rest generate-when-missing comment-string)
  "Extract the target from LINE using a regex that matches <<[anything]>>.
where [anything] is one or more characters. Return an Org mode link to the target."
  (let* ((line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (target (target-from-line line)))
    (cond (target (substring-no-properties target))
          (generate-when-missing 
           (let ((target (gen-id-tag)))
             (append-to-line line (concat (or comment-string " ;") target))
             target))
          (:no-target-found line))))

(defun create-link-target! () ;<id:1678574962>
  (let* ((root (projectile-project-root))
         (file buffer-file-name)
         (relative-file (concat "file:" (file-relative-name file root)))
         (link (let* ((line (line-number-at-pos (point-marker)))
                      (line-point (bounds-of-thing-at-point 'line)))
                 (list :line line 
                       :target ( extract-target-from-line! line t "  ;")
                       :text (remove-non-symbol-chars (buffer-substring (car line-point) (- (cdr line-point) 1))))))
         (line (plist-get link :line))
         (line-string (plist-get link :text))
         (target (plist-get link :target)))
    (list :link (format "[[%s::%s][%s]]" relative-file target line-string)
          :file file
          :target target)))

(defun current-project-buffers ()
  "Return a list of current-non-hidden buffers that are of the current project."
  (let ((project-dir (projectile-project-root))) 
    (cl-remove-if-not (lambda (buf)
                        (and (buffer-file-name buf)
                             (string-prefix-p project-dir (file-name-directory (buffer-file-name buf))))) 
                      (current-non-hidden-buffers))))

(defun find-file-line-link! () ;<id:1672243830>
  "Jump to the most recent project buffer;
Then prompt user to navigate to the code they want to include.

The returned plist contains the following keys:

:link - the 'org-mode' link string.
:file - the relative path to the file from the current project.
:target - the target of the link. This is an id like '<id:1172243297>'
          so that links still work after refactoring."
  (let ((return-buffer (current-buffer)))
    (switch-to-buffer (cadr (current-project-buffers)))
    (message (substitute-command-keys "Jump to the code you want to include, and press \\[exit-recursive-edit] to finish."))
    (recursive-edit)
    (let* ((root (projectile-project-root))
           (relative-file (concat "file:" (file-relative-name buffer-file-name root)))
           (code-buffer (current-buffer)))
      (switch-to-buffer return-buffer)
      (with-current-buffer code-buffer (create-link-target!)))))

(defun transclusion-link-from-target (target)
  (let* ((line-link target)
         (link (plist-get line-link :link))
         (file (plist-get line-link :file))
         
         (target-info (let ((buffer (get-or-create-buffer-for-file file)))
                        (with-current-buffer buffer
                          (list :lang (downcase (if (listp mode-name) (car mode-name) mode-name)))))))
    (let ((lang (plist-get target-info :lang)))
      (cond ((string= lang "org") (format "#+transclude: %s" link))
            (t (format "#+transclude: %s :src %s :thing-at-point %s" link lang thing-to-use))))))

(defun copy-transclusion-link ()
  (interactive)
  (kill-new (transclusion-link-from-target (create-link-target!))))

(defun add-to-narrative () ;<id:1678580234>
  (interactive)
  (let ((narrative (or current-narrative (set-narrative)))
        (code-buf (current-buffer)))
    (switch-to-buffer (get-or-create-buffer-for-file narrative))
    (message (substitute-command-keys "Go to where you want the transclution, and press \\[exit-recursive-edit]"))
    (recursive-edit)
    (insert (transclusion-link-from-target (with-current-buffer code-buf (create-link-target!))))))

(defun find-and-insert-transclusion () ;id:1672243297>
  "Find file and thing to transclude into current buffer."
  (interactive)
  (insert (transclusion-link-from-target (find-file-line-link!)))
  (org-transclusion-add)))

(defun insert-file-link () ;<id:1672242704>
  (interactive)
  (insert (plist-get (find-file-line-link) :link)))


(transient-define-suffix jorana-set-narrative (current-narrative)
  "Set the sentence from minibuffer read"
  :description '(lambda ()
                  (concat
                   "Set current narrative: "
                   (propertize
                    (format "%s" jorana-current-narrative)
                    'face 'transient-argument)))
  (interactive (list (read-file-name "Narrative: " jorana-current-narrative)))
  (setf jorana-current-narrative current-narrative))

(transient-define-suffix jorana-set-thing-to-use (thing)
  :description '(lambda ()
                  (concat "Thing to use:"
                          (propertize (format "%s" jorana-thing-to-use)
                                      'face 'transient-argument)))
  (interactive (completing-read-multiple "Thing-at-point to transclude: " '("sexp" "paragraph" "defun" "list" "sentence")))
  (setf jorana-thing-to-use thing))

;;;###autoload
(transient-define-prefix jorana-dashboard () ;<id:1678513393>
  "Create a sentence with several objects and a verb."
  ["Jorana Dashboard -- Variables"
   ("SPC" jorana-set-narrative :transient t)
   ("s" jorana-set-thing-to-use :transient t)]
  [["Actions"
    ("i" "find and include THING" jorana-find-and-insert-transclusion)
    ("a" "Add current THING to narrative" jorana-add-to-narrative)
    ("e" "Live edit the transclusion" org-transclusion-live-sync-start)]])

(provide 'jorana)
;;; jorana.el ends here
