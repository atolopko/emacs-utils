;;; Ruby and Rails development extensions for Emacs
;;;
;;; Author: atolopko
;;;
;;; To use add this to your .emacs:
;;; (require 'ruby-ext)
;;;
;;; suggested keyboard shortcuts
;;;
;;; (add-hook 'ruby-mode-hook
;;;           (lambda () 
;;;             (local-set-key [(control c) (control r) ?o] 'ruby-outline)
;;;             (local-set-key [(control c) (control r) ?m] 'ruby-check-buffer)
;;;             (local-set-kek [(control c) (control r) ?r] 'rgrep-rails)))

(defun ruby-outline() 
  (interactive)
  (occur "^ *\\(module\\|class\\|def\\|context\\|describe\\|it\\|scenario\\|share.*examples\\) +"))

(defun ruby-check-buffer()
  (interactive)
  (shell-command-on-region 1 (buffer-size) "ruby -c -w"))

(defun seq-drop-last (seq) (subseq seq 0 (1- (length seq))))

(defun strings-join (strings delimiter)
  (mapconcat 'identity strings delimiter))

(defun parent-directory (dir)
  (strings-join (seq-drop-last (split-string dir "/")) "/"))

(defun rails-root-dir-p (dir)
  (cl-find "Gemfile" (directory-files dir) :test 'string=))

(defun rails-project-root-dir (from)
  (if (or (string= "" from) (string= "/" from))
      nil
    (if (rails-root-dir-p from)
        from
      (rails-project-root-dir (parent-directory from)))))

(require 'grep)

(defun rgrep-rails (regexp)
  "Perform regexp search on current Ruby project files"
  (interactive
   (progn (grep-compute-defaults)
          (list (grep-read-regexp))))
  (rgrep 
   regexp
   "*.rb *.erb *.js *.css *.scss"
   (rails-project-root-dir default-directory)))

(provide 'ruby-ext)
