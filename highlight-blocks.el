;;; highlight-blocks.el --- Highlight the blocks point is in -*- lexical-binding: t -*-

;; Author: Fanael Linithien <fanael4@gmail.com>
;; URL: https://github.com/Fanael/highlight-blocks
;; Version: 0.1.2
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
;;   * Neither the name of the copyright holder(s) nor the names of any
;;     contributors may be used to endorse or promote products derived from
;;     this software without specific prior written permission.
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
  '((((class color) (background dark)) :background "gray20")
    (((class color) (background light)) :background "gray80"))
  "Current nested block face, depth 1."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-2-face
  '((((class color) (background dark)) :background "gray23")
    (((class color) (background light)) :background "gray77"))
  "Current nested block face, depth 2."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-3-face
  '((((class color) (background dark)) :background "gray26")
    (((class color) (background light)) :background "gray74"))
  "Current nested block face, depth 3."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-4-face
  '((((class color) (background dark)) :background "gray29")
    (((class color) (background light)) :background "gray71"))
  "Current nested block face, depth 4."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-5-face
  '((((class color) (background dark)) :background "gray32")
    (((class color) (background light)) :background "gray68"))
  "Current nested block face, depth 5."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-6-face
  '((((class color) (background dark)) :background "gray35")
    (((class color) (background light)) :background "gray65"))
  "Current nested block face, depth 6."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-7-face
  '((((class color) (background dark)) :background "gray38")
    (((class color) (background light)) :background "gray62"))
  "Current nested block face, depth 7."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-8-face
  '((((class color) (background dark)) :background "gray41")
    (((class color) (background light)) :background "gray59"))
  "Current nested block face, depth 8."
  :group 'highlight-blocks-faces)

(defface highlight-blocks-depth-9-face
  '((((class color) (background dark)) :background "gray44")
    (((class color) (background light)) :background "gray56"))
  "Current nested block face, depth 9."
  :group 'highlight-blocks-faces)

(defvar highlight-blocks--timer nil)
(make-variable-buffer-local 'highlight-blocks--timer)

(defvar highlight-blocks--overlays nil)
(make-variable-buffer-local 'highlight-blocks--overlays)

(defun highlight-blocks--delete-overlays ()
  "Delete all used overlays."
  (dolist (overlay highlight-blocks--overlays)
    (delete-overlay overlay))
  (setq highlight-blocks--overlays nil))

(defun highlight-blocks--make-overlay (depth beg end)
  "Make a new overlay.

DEPTH controls the face and priority, BEG and END are the positions in
buffer."
  (let ((overlay (make-overlay beg end)))
    (overlay-put overlay 'priority depth)
    (overlay-put overlay 'face (highlight-blocks--get-face depth))
    (push overlay highlight-blocks--overlays)))

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

(defun highlight-blocks--get-block-start (pos)
  "Get the beginning of the block at POS."
  (if (and (/= pos (point-min))
           (= ?\( (char-syntax (char-before pos))))
      (1- pos)
    (scan-lists pos -1 1)))

(defun highlight-blocks--get-block-end (pos)
  "Get the end of the block at POS."
  (if (and (/= pos (point-max))
           (= ?\) (char-syntax (char-after pos))))
      (1+ pos)
    (scan-lists pos 1 1)))

(defun highlight-blocks--get-bounds ()
  "Get the bounds of the nested blocks the point is in.

The returned value is a list of conses, where car is the start of a
block and cdr is the end of a block, starting from the outermost
block."
  (let ((result '()))
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
            (setq begin (highlight-blocks--get-block-start begin))
            (setq end (highlight-blocks--get-block-end end))
            (push (cons begin end) result)
            (setq i (1+ i))))
      (scan-error))
    result))

(defun highlight-blocks--fn ()
  "The main worker function of `highlight-blocks-mode'."
  (when highlight-blocks-mode
    (highlight-blocks--delete-overlays)
    (let ((i 1))
      (dolist (bounds (highlight-blocks--get-bounds))
        (highlight-blocks--make-overlay i (car bounds) (cdr bounds))
        (setq i (1+ i))))))

(defun highlight-blocks--mode-on ()
  "Turn on `highlight-blocks-mode'."
  (add-hook 'change-major-mode-hook 'highlight-blocks--mode-off nil t)
  (setq highlight-blocks--overlays nil)
  (setq highlight-blocks--timer (run-with-idle-timer
                                 highlight-blocks-delay t 'highlight-blocks--fn)))

(defun highlight-blocks--mode-off ()
  "Turn off `highlight-blocks-mode'."
  (remove-hook 'change-major-mode-hook 'highlight-blocks--mode-off t)
  (when highlight-blocks--timer
    (cancel-timer highlight-blocks--timer)
    (setq highlight-blocks--timer nil))
  (highlight-blocks--delete-overlays))

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

(provide 'highlight-blocks)
;;; highlight-blocks.el ends here
