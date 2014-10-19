;;; highlight-blocks.el --- Highlight the blocks point is in -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/highlight-blocks
;; Version: 0.1.10
;; Package-Requires: ((emacs "24"))

;; This file is NOT part of GNU Emacs.

;; Copyright (c) 2014, Fanael Linithien
;; All rights reserved.
;;
;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions are
;; met:
;;
;;   * Redistributions of source code must retain the above copyright
;;     notice, this list of conditions and the following disclaimer.
;;   * Redistributions in binary form must reproduce the above copyright
;;     notice, this list of conditions and the following disclaimer in the
;;     documentation and/or other materials provided with the distribution.
;;
;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS
;; IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED
;; TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A
;; PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER
;; OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
;; EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
;; PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
;; PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
;; LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
;; NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
;; SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; Commentary:
;; `highlight-blocks' provides highlighting of the blocks the point is currently
;; in.
;;
;; There are two modes of operation: volatile highlighting
;; (`highlight-blocks-now') and continuously updated highlighting (the
;; `highlight-blocks-mode' minor mode).

;;; Code:

(defgroup highlight-blocks nil
  "Highlight the paren-delimited blocks point is currently in."
  :prefix "highlight-blocks-"
  :group 'convenience)

(defcustom highlight-blocks-delay 0.1
  "Time in seconds to delay before highlighting blocks.
If you change this while `highlight-blocks-mode' is active, you must
toggle the mode off and on again for it to take effect."
  :type '(number :tag "seconds")
  :group 'highlight-blocks)

(defcustom highlight-blocks-now-time 10
  "How long should the blocks be highlighted for, in seconds
Only the highlighting done by `highlight-blocks-now' is affected."
  :type '(number :tag "seconds")
  :group 'highlight-blocks)

(defcustom highlight-blocks-max-innermost-block-count t
  "Maximum number of innermost blocks to highlight.
If t, don't limit."
  :type '(choice (const :tag "infinite" t)
                 integer)
  :group 'highlight-blocks)

(defvar highlight-blocks-max-face-count 9
  "Number of faces to use for highlighting current blocks.")

(defgroup highlight-blocks-faces nil
  "Faces for highlighting the blocks the points is in."
  :group 'highlight-blocks
  :group 'faces)

(defface highlight-blocks-depth-1-face
  '((((class color) (background dark)) :background "gray10")
    (((class color) (background light)) :background "gray90"))
  "Current nested block face, depth 1."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-2-face
  '((((class color) (background dark)) :background "gray13")
    (((class color) (background light)) :background "gray87"))
  "Current nested block face, depth 2."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-3-face
  '((((class color) (background dark)) :background "gray16")
    (((class color) (background light)) :background "gray84"))
  "Current nested block face, depth 3."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-4-face
  '((((class color) (background dark)) :background "gray19")
    (((class color) (background light)) :background "gray81"))
  "Current nested block face, depth 4."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-5-face
  '((((class color) (background dark)) :background "gray22")
    (((class color) (background light)) :background "gray78"))
  "Current nested block face, depth 5."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-6-face
  '((((class color) (background dark)) :background "gray25")
    (((class color) (background light)) :background "gray75"))
  "Current nested block face, depth 6."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-7-face
  '((((class color) (background dark)) :background "gray28")
    (((class color) (background light)) :background "gray72"))
  "Current nested block face, depth 7."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-8-face
  '((((class color) (background dark)) :background "gray31")
    (((class color) (background light)) :background "gray69"))
  "Current nested block face, depth 8."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-9-face
  '((((class color) (background dark)) :background "gray34")
    (((class color) (background light)) :background "gray66"))
  "Current nested block face, depth 9."
  :group 'highlight-blocks-faces)

;;;###autoload
(define-minor-mode highlight-blocks-mode
  "Highlight the nested blocks the point is currently in.

Toggle Highlight Blocks on or off.

With a prefix argument ARG, enable Highlight Blocks mode if ARG is
positive, and disable it otherwise. If called from Lisp, enable the
mode if ARG is omitted or nil, and toggle it if ARG is `toggle'."
  :init-value nil
  :lighter ""
  :keymap nil
  (highlight-blocks--mode-off)
  (when highlight-blocks-mode
    (highlight-blocks--mode-on)))

;;;###autoload
(defun highlight-blocks-now (&optional howmany)
  "Highlight the nested blocks the point is in for `highlight-blocks-now-time'
seconds, or until input is available.
When called with an prefix argument, its value determines how many of the
innermost blocks will be highlighted; when called with no argument, the value
`highlight-blocks-max-innermost-block-count' is used, which see."
  (interactive "P")
  ;; So we don't leave overlays lying around upon `keyboard-quit'.
  (unwind-protect
      (progn
        (let ((highlight-blocks-max-innermost-block-count
               (if howmany
                   (prefix-numeric-value howmany)
                 highlight-blocks-max-innermost-block-count)))
          (highlight-blocks--update-selected-window))
        (sit-for highlight-blocks-now-time))
    (highlight-blocks--delete-overlays)))

(defvar highlight-blocks--original-delay nil
  "Delay used in this buffer.")
(defvar highlight-blocks--timers (make-hash-table :test #'eql)
  "Hash table of delay => (refcount . timer).
The delay is used to ensure it's still possible to use different delays in
different buffers.")

(defun highlight-blocks--delete-window-overlays (window)
  "Delete all used overlays in the WINDOW."
  (mapc #'delete-overlay (window-parameter window 'highlight-blocks--overlays))
  (set-window-parameter window 'highlight-blocks--overlays nil))

(defun highlight-blocks--delete-overlays ()
  "Delete all used overlays in all windows showing the current buffer."
  (mapc #'highlight-blocks--delete-window-overlays (get-buffer-window-list nil nil t)))

(defun highlight-blocks--make-overlay (depth beg end window)
  "Make a new overlay.

DEPTH controls the face and priority, BEG and END are the positions in
buffer, WINDOW is the window to show the overlay in."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'window window)
    (overlay-put overlay 'priority depth)
    (overlay-put overlay 'face (highlight-blocks--get-face depth))
    (set-window-parameter window 'highlight-blocks--overlays
                          (cons overlay
                                (window-parameter window 'highlight-blocks--overlays)))))

(defun highlight-blocks--get-face (depth)
  "Get the face corresponding to the (1-based) DEPTH."
  (intern-soft
   (concat "highlight-blocks-depth-"
           (number-to-string
            (if (<= depth highlight-blocks-max-face-count)
                depth
              (+ 1 (mod (- depth highlight-blocks-max-face-count 1)
                        highlight-blocks-max-face-count))))
           "-face")))

(defun highlight-blocks--get-bounds ()
  "Get the bounds of the nested blocks the point is in.

The returned value is a list of conses, where car is the start of a
block and cdr is the end of a block, starting from the outermost
block."
  (let ((result '())
        (parse-sexp-ignore-comments t))
    (condition-case nil
        (let* ((parsestate (syntax-ppss))
               (startingpos (if (or (nth 3 parsestate)
                                    (nth 4 parsestate))
                                (nth 8 parsestate)
                              (point)))
               (begin startingpos)
               (end startingpos)
               (i 0))
          (while (or (eq highlight-blocks-max-innermost-block-count t)
                     (< i highlight-blocks-max-innermost-block-count))
            (setq begin (scan-lists begin -1 1))
            (setq end (scan-lists end 1 1))
            (push (cons begin end) result)
            (setq i (1+ i))))
      (scan-error))
    result))

(defun highlight-blocks--update-selected-window ()
  "Highlight blocks in the selected window."
  (let ((window (selected-window)))
    (highlight-blocks--delete-window-overlays window)
    (let ((i 1))
      (dolist (bounds (highlight-blocks--get-bounds))
        (highlight-blocks--make-overlay i (car bounds) (cdr bounds) window)
        (setq i (1+ i))))))

(defun highlight-blocks--update-current-buffer ()
  "Highlight blocks in all windows displaying the current buffer.
This is the main worker function of `highlight-blocks-mode'."
  (when highlight-blocks-mode
    (dolist (window (get-buffer-window-list nil nil t))
      (with-selected-window window
        (highlight-blocks--update-selected-window)))))

(defun highlight-blocks--mode-on ()
  "Turn on `highlight-blocks-mode'."
  (add-hook 'change-major-mode-hook #'highlight-blocks--mode-off nil t)
  (add-hook 'kill-buffer-hook #'highlight-blocks--mode-off nil t)
  (set (make-local-variable 'highlight-blocks--original-delay) highlight-blocks-delay)
  (let ((timerbucket (gethash highlight-blocks-delay highlight-blocks--timers)))
    (if timerbucket
        (setcar timerbucket (1+ (car timerbucket)))
      (puthash highlight-blocks-delay
               (cons 1 (run-with-idle-timer highlight-blocks-delay
                                            t
                                            #'highlight-blocks--update-current-buffer))
               highlight-blocks--timers))))

(defun highlight-blocks--mode-off ()
  "Turn off `highlight-blocks-mode'."
  (remove-hook 'change-major-mode-hook #'highlight-blocks--mode-off t)
  (remove-hook 'kill-buffer-hook #'highlight-blocks--mode-off t)
  (highlight-blocks--delete-overlays)
  (when (local-variable-p 'highlight-blocks--original-delay)
    (let* ((originaldelay highlight-blocks--original-delay)
           (timerbucket (gethash originaldelay highlight-blocks--timers)))
      (when timerbucket
        (let ((refcount (car timerbucket)))
          (if (> refcount 1)
              (setcar timerbucket (1- refcount))
            (cancel-timer (cdr timerbucket))
            (remhash originaldelay highlight-blocks--timers)))))
    (kill-local-variable 'highlight-blocks--original-delay)))

(provide 'highlight-blocks)
;;; highlight-blocks.el ends here
