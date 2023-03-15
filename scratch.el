;; This buffer is for text that is not saved, and for Lisp evaluation.
;; To create a file, visit it with SPC . and enter text in its buffer.

(message "Text is read-only")

(defvar my-last-command nil)

(defun my-pre-command-function ()
  "Log the last command."
  (message (format "%s" this-command)))

(defun read-only-jump-before-edit (data context caller)
  (if (get-char-property (point) 'read-only)
      (let* ((last-user-cmds (recent-keys 'recent-cmds))
             (last-user-cmd (cdr (aref last-user-cmds (1- (length last-user-cmds)))))
             (in-transclusion (get-char-property (point) 'org-transclusion-type))
             (active-state evil-state))
        (if in-transclusion
            (progn (call-interactively #'jorana-jump-to-transclusion-pair)
                   (cond ((eq last-user-cmd #'self-insert-command) (self-insert-command 1))
                         ((eq last-user-cmd #'org-return) (newline-and-indent))
                         (t (call-interactively last-user-cmd)))
                   ;; (when (eq active-state 'symex)
                   ;;   (run-with-timer 0.01 nil #'symex-mode-interface))
                   )
          (funcall #'help-command-error-confusable-suggestions data context caller)))
    (funcall #'help-command-error-confusable-suggestions data context caller)))

(setq command-error-function #'read-only-jump-before-edit)


