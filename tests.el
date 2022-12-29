
 (defun toggle-id-regex ();<bob bob>
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

(org-element-at-point)
