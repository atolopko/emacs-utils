;; traverse-buffers.el --- quickly move back and forth through buffers
;;
;; Copyright (C) 2001, Andrew Tolopko
;;
;; Maintainer: Andrew Tolopko
;; Keywords: lisp, buffer, buffers, traverse, switch
;; Version: 0.1
;;
;;
;; Description:
;;
;; This package allows the user to traverse back and forth through
;; buffers with quick, simple key strokes.  Traversal follows the
;; order of the buffer history list (as reported by the `buffer-list' 
;; function).
;;
;;
;; Motivation:
;;
;; Often the user will be editing multiple files and will want to
;; quickly switch to a previously visited buffer. While the 'Control-x
;; b' (switch-to-buffer) command allows the user to quickly visit the
;; most recently visited buffer, to visit a less recently visited
;; buffer, the user must type the name of the buffer to visit. While
;; this is generally usable, the user may find that this technique
;; somewhat cumbersome while engaged in a mad coding frenzy. The user
;; may also find it hard to remember the name of a buffer that was
;; visited three or four buffers ago (even with the auto completion
;; feature, this can be difficult when multiple files share prefixes).
;; 
;;
;; Operation:
;;
;; By default, this package enlists the 'Meta-[' and 'Meta-]' key
;; combinations to traverse through the buffer history list. Buffers
;; that start with " *" or "*" are ignored, as these buffers are not
;; generally "user" buffers, but instead have contents that have been
;; programatically generated. As the user traverses through the
;; buffers, the order of the buffer history list is left unchanged.
;; Thus two consecutive 'Meta-[' key sequences will visit the second
;; most recently visited buffer, while three consecutive 'Meta-[' key
;; sequences will visit the third most recently visited buffer, and so
;; on.  Contrast this with the switch-to-buffer command where any
;; number of invocations will simply toggle between only two
;; buffers--the "original" buffer and the most-recently visited
;; buffer. The 'Meta-]' key sequence will cause buffer traversal to
;; occur in the opposite direction. Anyone who is familiar with the
;; operation of the Alt-Tab key sequence in Windows or Gnome will be
;; familiar with the operation of the 'Meta-[' and 'Meta-]' keys. For
;; example:
;;
;; Assume four buffers: A, B, and C, where A is the current buffer, B
;; is the next most-recently edited buffer and C is the least recently
;; visited buffer. Pressing 'Meta-[' will cause buffer B to be
;; displayed. Immediately pressing 'Meta-[' again will cause buffer C
;; to be displayed. At this point pressing 'Meta-]' will cause buffer
;; B to be displayed. If the user types anything else at this point
;; (e.g., edits the buffer), the next 'Meta-[' sequence will cause
;; buffer A to be displayed (and not buffer C, because the order of
;; the buffer history list was not changed until the user edited
;; buffer B!)
;;
;;
;; For Developers:
;;
;; Please note that the 'Meta-[' and 'Meta-]' key sequences are
;; hardcoded into the `in-buffer-traversal' function. This is not
;; ideal and should be changed.

;;; Code:
(defun user-buffer-names (buffer-names)
  "Returns a list of buffer names, excluding any that start with \"*\" or \" *\""
  (defun all-true (pred l)
    (if (null l) 
        t
      (and (apply pred (car l) nil) (all-true pred (cdr l)))))
  (let ((name (buffer-name (car buffer-names)))
        (exclude-buffers '("^ \\*"))) ; exclude non-user-generated buffers
    (if buffer-names
        (if (not (all-true (lambda (exclude-buffer) 
                             (numberp (string-match exclude-buffer name)))
                           exclude-buffers))
            ; add this buffer to the returned list
            (cons name
                  (user-buffer-names (cdr buffer-names)))
          ; skip this buffer
          (user-buffer-names (cdr buffer-names))))))

(defun next-element (list e)
  "Returns the element occuring after 'e'. If 'e' 
is the last element in 'list', the first element is returned. If 
'e' does not exist in 'list', the first element is returned."
  (let ((n 0) (len (length list)))
    (while (and (not (string= e (nth n list)))
                (< n len))
      (setq n (+ n 1)))
    (cond ((= n (- len 1)) (nth 0 list)) ; if elem is last, cycle to first
          ((= n len) (nth 0 list)) ; elem not found in list
          (t (nth (+ n 1) list)))))

(defun next-command-event-silent ()
  "Similar to next-command-event, but does not feel the need to display
the keystrokes in the minibuffer."
 (let '(event (make-event))
   (while (progn
            (next-event event)
            (not (or (key-press-event-p event)
                     (button-press-event-p event)
                     (button-release-event-p event)
                     (misc-user-event-p event))))
     (dispatch-event event))
   event))

(defun in-buffer-traversal ()
  "Reads the next command event to determine if the user is still traversing
buffers."
  (let ((evt (next-command-event-silent))
        (prev-key (make-event 'key-press '(key ?\] modifiers (meta))))
        (next-key (make-event 'key-press '(key ?\[ modifiers (meta)))))
    (setq unread-command-events (list evt))
    (or (equal prev-key evt)
        (equal next-key evt))))

(defun previous-user-buffer ()
  "Switch to the previous buffer in the buffer history list without changing the 
order of the history list."
  (interactive)
  (let ((previous-buffer (next-element (user-buffer-names (buffer-list))
                                       (buffer-name))))
    (switch-to-buffer previous-buffer t)
    (if (not (in-buffer-traversal))
        (switch-to-buffer previous-buffer nil))))

(defun next-user-buffer ()
  "Switch to the next buffer in the buffer history list without changing the 
order of the history list."
  (interactive)
  (let ((next-buffer (next-element (reverse (user-buffer-names (buffer-list)))
                                   (buffer-name))))
    (switch-to-buffer next-buffer t)
    (if (not (in-buffer-traversal))
        (switch-to-buffer next-buffer nil))))

; set the keys we'll use for traversing buffers we probably should
; allow the user to do this in his .emacs file, but until we figure
; out a way of determining the key sequences that have been associated
; with `previous-user-buffer' and `next-user-buffer' (thus allowing
; the hardcoded key sequences from `in-buffer-traversal' to be
; removed), we'll take care of it here
(global-set-key "\M-[" 'previous-user-buffer)
(global-set-key "\M-]" 'next-user-buffer)


(provide 'traverse-buffers)