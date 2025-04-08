;;; md-babel.el --- Execute Markdown blocks  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze <post@christiantietze.de>
;; Version: 0.1
;; Package-Requires: ((markdown-mode "2.0") (emacs "27.1"))
;; Keywords: Markdown
;; URL: https://md-babel.org/

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides functionality to execute code blocks in
;; Markdown documents via the `md-babel' executable.
;;
;; Set `md-babel-path' (until auto-discovery is implemented).


;;; Code:

(defvar md-babel-path
  nil
  "Path to your `md-babel' executable.")

(defun md-babel--source-location-at-point ()
  "Returns the cmark-compatible source location of point, 1-based."
  (let ((line (line-number-at-pos))
        (column (current-column)))
    (cons line (+ 1 column))))

(defun md-babel--source-location-line (location)
  (car location))
(defun md-babel--source-location-column (location)
  (cdr location))

(defvar md-babel--result-buffer-name "*md-babel-result*")

;; swift run md-babel exec --file test.txt --line 7 --column 1
(defun md-babel--execute-command (file location)
  "Assembles the shell invocation to execute FILE at LOCATION."
  (mapconcat
   (lambda (p) (format "%s" p))
   (list md-babel-path
        "exec"
        "--file" (format "\"%s\"" file)
        "--line" (md-babel--source-location-line location)
        "--column" (md-babel--source-location-column location))
   " "))

(defun md-babel-execute-block-at-point ()
  (interactive)
  (when-let* ((location (md-babel--source-location-at-point))
              (file (buffer-file-name))
              (result (md-babel--execute file location)))
    (save-excursion
      (md-babel--mark-range (alist-get 'replacementRange result))
      (call-interactively #'delete-region)
      (insert (alist-get 'replacementString result))
      )))

(defun md-babel--mark-range (range-alist)
  "Mark range from RANGE-ALIST in current buffer.

RANGE-ALIST is expected to be of the form:

    (from ((line . FROM-LINE) (column . FROM-COLUMN))
     to ((line . TO-LINE) (column . TO-COLUMN)))
"
  (md-babel--move-point (alist-get 'from range-alist))
  (push-mark (point) t t)
  (md-babel--move-point (alist-get 'to range-alist)))

(defun md-babel--move-point (location-alist &OPTIONAL buffer)
  "Move point in BUFFER to absolute location from LOCATION-ALIST.

LOCATION-ALIST is expected to be of the form:

    ((line . LINE) (column . COLUMN))

Uses current buffer if BUFFER is nil.
"
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (beginning-of-line (alist-get 'line location-alist))
    (move-to-column (- (alist-get 'column location-alist) 1))))

(defun md-babel--execute (file location)
  "Instructs md-babel to execute block at LOCATION in FILE.

Returns the alist form of the JSON response.

The program’s JSON response is inserted into a buffer with the name
`md-babel--result-buffer-name', which see."
  (let* ((command (md-babel--execute-command file location))
         (json-str (with-temp-buffer
                     (shell-command command (current-buffer) nil)
                     (buffer-substring-no-properties (point-min) (point-max))
                     ))
         (result-buffer (get-buffer-create md-babel--result-buffer-name)))
    (with-current-buffer result-buffer
      (delete-region (point-min) (point-max))
      (insert json-str))
    (json-parse-string json-str :object-type 'alist)))


;;;###autoload

(provide 'md-babel)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
;;; md-babel.el ends here
