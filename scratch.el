;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with SPC . and enter text in its buffer.

(message "Text is read-only")

(defvar my-last-command nil)

(defun my-pre-command-function ()
  "Log the last command."
  (message (format "%s" this-command)))

(defun p4-write-attempt (data context caller) 										; If it's a read-only-buffer and you wrote to it
  (if (get-char-property (point) 'read-only) 																		; And the user wants to checked out
      (let* ((last-user-cmds (recent-keys 'recent-cmds))
             (last-user-cmd (cdr (aref last-user-cmds (1- (length last-user-cmds)))))
             (in-transclusion (get-char-property (point) 'org-transclusion-type)))
        (if in-transclusion
            (progn (call-interactively #'jump-to-transclusion-pair)
                   (if (eq last-user-cmd #'self-insert-command)
                       (self-insert-command 1)
                     (call-interactively last-user-cmd)))
          (funcall #'help-command-error-confusable-suggestions data context caller)))
    (funcall #'help-command-error-confusable-suggestions data context caller)))

(setq command-error-function #'p4-write-attempt)
command-error-function-bak

(remove-hook 'pre-command-hook #'my-pre-command-function)
(view-lossage)
(setf my-array (recent-keys 'recent-cmds))
(aref my-array (1- (length my-array)))
