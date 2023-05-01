;;; jorana --- Summary
;;; Package-Requires: ((names "") (transient "") (consult "") (projectile "") (emacs "24"))



;;; Commentary:
;;; Code:

(provide 'jorana)


(use-package transient)

(defcustom jorana-current-narrative nil ;<id:1678875463>
  "Define the narrative for the current project.")

(defvar jorana-id-regex (concat "\\(#_\\|;\\|//\\|\\)\\( ?\\)\(?<id:.*?>\)?")
  "Regex that matches target tag comments.

   Example matches:
   #_<id:tag>
   #_(<id:tag>)
   ; <id:tag>
   //<id:tag>
   <id:tag>")

(defvar jorana-thing-to-use "sexp")

(defmacro jorana-alist-of-let* (let-bindings &rest body)
  "This is the same as #'let*, except it return an alist of the bindings.
LET-BINDINGS and BODY are the same as in #'let*."
  `(let* ,let-bindings
     ,@body
     (list ,@(mapcar (lambda (binding)
                       (list #'cons
                             `',(car binding)
                             (car binding)))
                     let-bindings))))

(defun jorana-get-or-create-buffer-for-file (filepath)
  "Return the buffer for FILEPATH, or open buffer if it does not yet exist.
We don't just use 'find-file-noselect because it would not include unsaved changes."
  (or (find-buffer-visiting filepath)
      (find-file-noselect filepath)))

(defun jorana-current-time-in-seconds ()
  "Return the current time in seconds from the epoch."
  (format-time-string "%s" (current-time)))

(defun jorana-get-line-contents (line)
  "Return the line contents for LINE, with leading and trailing whitespace trimmed."
  (let ((start (line-beginning-position line))
        (end (line-end-position line)))
    (string-trim (buffer-substring start end))))

(defun jorana-remove-non-symbol-chars (string)
  "Replace all non link-safe characters in STRING with underscores."
  (replace-regexp-in-string "[^a-zA-Z0-9_]" "_" string))


;;; * ids

(defun jorana-gen-id-tag ()
  (format "<id:%s>" (jorana-current-time-in-seconds)))

(defun jorana-insert-id-tag () ;<id:1672227875>
  "Insert a jorana id. Currently, the current time in seconds from the epoch."
  (interactive)
  (insert (jorana-gen-id-tag)))

(defun jorana-hide-id-regex () ;<id:1672243157>
  "Hide."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward jorana-id-regex nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
    (set-buffer-modified-p nil)))

(defun jorana-toggle-id-visibility () ;
  "Toggle the visibility id markers IE. `#_(id:<number>)`."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward jorana-id-regex nil t)
      (let ((invisible (get-text-property (match-beginning 0) 'invisible)))
        (if invisible
            (remove-text-properties (match-beginning 0) (match-end 0) '(invisible t))
          (add-text-properties (match-beginning 0) (match-end 0) '(invisible t))))
      (set-buffer-modified-p nil))))

(defun jorana-hide-id-regex () ;<id:1672242977>
  "Hide."
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward jorana-id-regex nil t)
      (add-text-properties (match-beginning 0) (match-end 0) '(invisible t)))
    (set-buffer-modified-p nil)))

;;; * misc

(defun jorana-current-non-hidden-buffers ()
  "Return a list of the current non-hidden buffers."
  (interactive)
  (let ((buffers (buffer-list)))
    (seq-filter (lambda (buffer) (not (string-prefix-p " " (buffer-name buffer)))) buffers)))
(defun jorana-current-project-buffers ()
  "Return a list of current-non-hidden buffers that are of the current project."
  (let ((project-dir (projectile-project-root))) 
    (cl-remove-if-not (lambda (buf)
                        (and (buffer-file-name buf)
                             (string-prefix-p project-dir (file-name-directory (buffer-file-name buf))))) 
                      (jorana-current-non-hidden-buffers))))

(defun jorana-create-org-link-to-line ()
  "Create an Org mode link to the contents of the line at point."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (line-number (line-number-at-pos))
         (line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (link-text (format "[[%s::%s][%s]]" file-name line-text line-text)))
    (kill-new link-text)))

;;; * marker utils

(defun jorana-marker-at-line (line) ;<id:1672242453>
  "Create a marker at the beginning of LINE in the current buffer."
  (set-marker (make-marker) (line-beginning-position line)))

(defun jorana-marker-at-point (point &rest buffer) ;<id:1672242453>
  "Create a marker at POINT in BUFFER."
  (set-marker (make-marker) point (car (or buffer (current-buffer)))))

(defun jorana-append-to-line (line string)
  "Append STRING to the end of LINE in the current buffer."
  (end-of-line)
  (insert string))

(defun jorana--goto-marker (marker)
  (switch-to-buffer (marker-buffer marker))
  (goto-char marker))
;;; -- 
;;; * Target

(defun jorana-target-at-point ()
  (interactive)
  (jorana-extract-target-from-line (line-number-at-pos (point)) t)
  )

(defun jorana-target-from-line (line) ;<id:1678573021>
  "Target from LINE."
  (let ((on-org-heading (and (eq major-mode 'org-mode)
                             (org-on-heading-p))))
    (if on-org-heading
        (let* ((org-id (org-id-get-create)))
          (org-entry-put nil "CUSTOM_ID" org-id)
          (put-text-property 0 (length org-id) :type 'org org-id)
          org-id)
      ;; get id from line if it already exists
      (prog2 (string-match "\\(<.*>\\)" line-text)
          (match-string 1 line-text)))))

(defun jorana-extract-target-from-line! (line &optional generate-when-missing comment-string) ;<id:1678630244>
  "Extract the target from LINE using a regex that matches a jorana-id.
GENERATE-WHEN-MISSING adds an id if one doesn't already exist. 
COMMENT-STRING is the comment to use to prefix the id.
where [anything] is one or more characters. Return an Org mode link to the target."
  (let* ((line-text (buffer-substring (line-beginning-position) (line-end-position)))
         (target (jorana-target-from-line line)))
    (cond (target target)
          (generate-when-missing 
           (let ((target (jorana-gen-id-tag)))
             (unless (eq major-mode 'org-mode)
               (jorana-append-to-line line (concat (or comment-string " ;") target)))
             target))
          (:no-target-found line))))

(defun jorana-create-link-target! () ;<id:1678574962>
  (let* ((root (projectile-project-root))
         (file buffer-file-name)
         (relative-file (concat "file:" (file-relative-name file root)))
         (link (let* ((line (line-number-at-pos (point-marker)))
                      (line-point (bounds-of-thing-at-point 'line)))
                 (list :line line 
                       :target (jorana-extract-target-from-line! line t "  ;")
                       :text (jorana-remove-non-symbol-chars (buffer-substring (car line-point) (- (cdr line-point) 1))))))
         (line (plist-get link :line))
         (line-string (plist-get link :text))
         (target (plist-get link :target))
         (target-type (get-text-property 0 :type target))
         (id-or-search (if (eq target-type 'org) "::#" "::")))
    (list :link (format "[[%s%s%s][%s]]" relative-file id-or-search (substring-no-properties target) line-string)
          :file file
          :target target)))

;;; * Jumping

(defun jorana-marker-of-mirrored-point (mirror-start cursor-offset)
  "Create marker to matching transclusion."
  (save-excursion
    (with-current-buffer (marker-buffer mirror-start)
      (goto-char mirror-start)
      (goto-char (+ (point) cursor-offset))
      (point-marker))))

(defun jorana-transclusion-info () ;<id:1678859998>
  (jorana-alist-of-let*
   ((transcluder (get-char-property (point) 'org-transclusion-by))
    (tc-pair (get-char-property (point) 'org-transclusion-pair))
    (transcludee (save-mark-and-excursion
                   (with-current-buffer (overlay-buffer tc-pair)
                     (goto-char (overlay-start tc-pair))
                     (point-marker))))
    ;; If this changes upstream we will need to change this logic accordingly;
    ;; When org-transclusion-by is present, we are at source.
    ;; Otherwise we are A. not in a transclusion at all,
    ;; or B. at the transcluder.
    (at-transcluder (and transcludee (not transcluder)))
    (mirror-start (or transcluder transcludee))
    (in-src-block (equal "src" (get-char-property (point) `org-transclusion-type)))
    (mirror-in-src-block (with-current-buffer (marker-buffer mirror-start)
                           (equal "src" (get-char-property mirror-start `org-transclusion-type))))
    (current-start (if (and at-transcluder in-src-block)
                       (save-mark-and-excursion
                         (org-babel-mark-block)
                         (set-marker (make-marker) (region-beginning)))
                     transcludee))
    (mirror-start (if mirror-in-src-block
                      (with-current-buffer (marker-buffer mirror-start)
                        (save-mark-and-excursion
                          (goto-char mirror-start)
                          (org-babel-mark-block)
                          (set-marker (make-marker) (region-beginning))))
                    mirror-start)))))

(defun jorana--mirror-offset (current-start)
  (with-current-buffer (marker-buffer current-start)
    (- (point) current-start)))

(defun jorana-refresh-remote-transclusion ()
  (let ((remote (get-char-property (point) 'org-transclusion-by)))
    (when remote
      (with-current-buffer (marker-buffer remote)
        (save-excursion
          (goto-char remote)
          (org-transclusion-refresh))))))

(defun jorana-jump-to-transclusion-pair () ;<id:1678874579>
  "Goto matching transclusion."
  (interactive)
  (jorana-refresh-remote-transclusion)
  ;; Let's try to not jar the view too much.
  (let ((current-scroll-pos (count-lines (window-start) (point))))
    (let-alist (jorana-transclusion-info)
      ;; try to re-trigger fontlock coloring by scrolling up first.
      (jorana--goto-marker .mirror-start)
      (recenter -1)
      (jorana--goto-marker (jorana-marker-of-mirrored-point .mirror-start (jorana--mirror-offset .current-start)))
      (recenter current-scroll-pos))))


;;; * Linking
(defun jorana-find-file-line-link! () ;<id:1672243830>
  "Jump to the most recent project buffer;
Then prompt user to navigate to the code they want to include.

The returned plist contains the following keys:

:link - the 'org-mode' link string.
:file - the relative path to the file from the current project.
:target - the target of the link. This is an id like '<id:1172243297>'
          so that links still work after refactoring."
  (let ((return-buffer (current-buffer)))
    (switch-to-buffer (cadr (jorana-current-project-buffers)))
    (message (substitute-command-keys "Jump to the code you want to include, and press \\[exit-recursive-edit] to finish."))
    (recursive-edit)
    (let* ((root (projectile-project-root))
           (relative-file (concat "file:" (file-relative-name buffer-file-name root)))
           (code-buffer (current-buffer)))
      (switch-to-buffer return-buffer)
      (with-current-buffer code-buffer (jorana-create-link-target!)))))

(defun jorana-transclusion-link-from-target (target)
  (let* ((line-link target)
         (link (plist-get line-link :link))
         (file (plist-get line-link :file))
         
         (target-info (let ((buffer (jorana-get-or-create-buffer-for-file file)))
                        (with-current-buffer buffer
                          (list :lang (downcase (if (listp mode-name) (car mode-name) mode-name)))))))
    (let ((lang (plist-get target-info :lang)))
      (cond ((string= lang "org") (format "#+transclude: %s" link))
            (t (format "#+transclude: %s :src %s :thing-at-point %s" link lang jorana-thing-to-use))))))

(defun jorana-copy-transclusion-link ()
  (interactive)
  (kill-new (jorana-transclusion-link-from-target (jorana-create-link-target!))))

(defun jorana-add-to-narrative () ;<id:1678580234>
  (interactive)
  (let ((narrative (or jorana-current-narrative (jorana-set-narrative (read-file-name "Narrative: " jorana-current-narrative))))
        (code-buf (current-buffer)))
    (switch-to-buffer (jorana-get-or-create-buffer-for-file narrative))
    (message (substitute-command-keys "Go to where you want the transclution, and press \\[exit-recursive-edit]"))
    (recursive-edit)
    (insert (jorana-transclusion-link-from-target (with-current-buffer code-buf (jorana-create-link-target!))))
    (org-transclusion-add)))

(defun jorana-find-and-insert-transclusion ()  ;<id:1678618587>
  "Find file and thing to transclude into current buffer."
  (interactive)
  (insert (jorana-transclusion-link-from-target (jorana-find-file-line-link!)))
  (org-transclusion-add))

(defun jorana-insert-file-link () ;<id:1672242704>
  (interactive)
  (insert (plist-get (jorana-find-file-line-link) :link)))



;;; * User interface

(transient-define-suffix jorana-set-narrative (jorana-current-narrative)
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
                  (concat "Thing to use: "
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
    ("f" "find and include THING" jorana-find-and-insert-transclusion)
    ("a" "Add current THING to narrative" jorana-add-to-narrative)
    ("e" "Live edit the transclusion" org-transclusion-live-sync-start)
    ("j" "Jump between source and transclusion" jorana-jump-to-transclusion-pair)]
   ["View"
    ("ta" "Transclude All" org-transclusion-add-all)
    ("tt" "Transclude link-at-point" org-transclusion-add)
    ("ra" "Remove all transclusions" org-transclusion-remove-all)
    ("rr" "Remove transclusion at-point" org-transclusion-remove)]])

(provide 'jorana)
;;; jorana.el ends here
