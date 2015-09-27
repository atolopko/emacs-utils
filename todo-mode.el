;; TODO: 
;;   - should select entire paragraph (but mark-paragraph selects
;;     paragraph's preceding empty line, rather than trailing new line,
;;     which is what we want)
;;   - prefix arg should prompt for date created/completed
;;   - command that prompts for a todo item in the minibuffer and appends 
;;     to a default .todo file
;;   - key bindings for setting color (i.e., priority) of todo items

(require 'time-date)

(define-derived-mode todo-mode text-mode "Todo List"
  "Major mode used for editing todo lists.
\\{todo-mode-map}"
  (define-key todo-mode-map "\C-c\C-c" 'todo-item-at-point-completed)
  (define-key todo-mode-map "\C-c\C-n" 'todo-new-item)
  (define-key todo-mode-map "\C-c\C-q" 'bury-buffer))

(defvar todo-completed-token "Completed:"
  "*The regular expression that defines the start of the section where completed todo items will be placed.")

(defvar todo-date-format "%m-%d-%y"
  "*The format string used to format todo items' creation and completion dates")

(defun todo-new-item ()
  (interactive)
  (beginning-of-line)
  ;(goto-char (point-min))
  (insert "[" (format-time-string todo-date-format (current-time)) "] \n")
  (backward-char))

(defun todo-item-at-point-completed (&optional days-prior-bias)
  (interactive "p")
  (save-excursion
   (goto-char (point-at-bol))
   (let* ((ins-pt (search-forward "]" (point-at-eol) t)))
     (if (not ins-pt) (progn 
                        (insert "[n/a] ")
                        (backward-char 1)))
     (backward-char 1)
     (insert ", " (format-time-string todo-date-format (current-time)))
;                                      (days-to-time (+ (time-to-days (current-time)) 
;                                                       days-prior-bias))))
     (goto-char (point-at-bol))
     (kill-line 1)
     (if (search-forward-regexp todo-completed-token nil t)
         (progn
           (next-line 1)
           (goto-char (point-at-bol)))
       (progn 
         (goto-char (point-max))
         (insert "\n" todo-completed-token "\n")))
     (yank))))

(provide 'todo-mode)