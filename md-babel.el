;;; md-babel.el --- Execute Markdown blocks  -*- lexical-binding:t -*-

;; Copyright (C) 2025 Christian Tietze

;; Author: Christian Tietze <post@christiantietze.de>
;; Version: 0.2.1
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


;;;; Configuration

;; TODO: Change to defcustom: https://github.com/md-babel/md-babel.el/issues/1
(defvar md-babel-path
  nil
  "Path to your `md-babel' executable.")

(defvar md-babel-response-buffer-name
  "*md-babel-response*"
  "Name of buffer to store JSON response from the `md-babel' program.")



;;;; Data types
;;;;; cmark's source location and source range

(defun md-babel--source-location-at-point (&optional buffer)
  "The cmark-compatible source location of `point' in BUFFER, 1-based.

A source location is the cons of 1-based line and column.

Uses the current buffer if BUFFER is nil."
  (let ((buffer (or buffer (current-buffer))))
    (with-current-buffer buffer
      (let ((line (line-number-at-pos))
            (column (current-column)))
        (cons line (+ 1 column))))))

(defun md-babel--source-location-line (location)
  (car location))
(defun md-babel--source-location-column (location)
  (cdr location))


;;;; Interpret responses

(defun md-babel--move-point (location-alist &optional buffer)
  "Move point in BUFFER to absolute location from LOCATION-ALIST.

LOCATION-ALIST is expected to be of the form:

    ((line . LINE) (column . COLUMN))

Uses current buffer if BUFFER is nil."
  (with-current-buffer (or buffer (current-buffer))
    (goto-char (point-min))
    (beginning-of-line (alist-get 'line location-alist))
    (move-to-column (- (alist-get 'column location-alist) 1))))

(defun md-babel--mark-range (range-alist)
  "Mark range from RANGE-ALIST in current buffer.

RANGE-ALIST is expected to be of the form:

    (from ((line . FROM-LINE) (column . FROM-COLUMN))
     to ((line . TO-LINE) (column . TO-COLUMN)))
"
  (md-babel--move-point (alist-get 'from range-alist))
  (push-mark (point) t t)
  (md-babel--move-point (alist-get 'to range-alist)))

(defun md-babel--interpret-response (response)
  (md-babel--mark-range (alist-get 'replacementRange response))
  (call-interactively #'delete-region)
  (insert (alist-get 'replacementString response))
  ;; Restore point (or range)
  (md-babel--mark-range (alist-get 'range response)))


;;;; Buffer content-based invocation

(defun md-babel--make-execute-stdin-command (location)
  "Assembles the invocation process standard input to execute LOCATION."
  (let ((line (number-to-string (md-babel--source-location-line location)))
        (column (number-to-string (md-babel--source-location-column
                                   location))))
    (string-join
     (list md-babel-path
           "exec"
           "--line" (shell-quote-argument line)
           "--column" (shell-quote-argument column))
     " ")))

(defun md-babel--execute-buffer (location &optional buffer)
  "Instructs md-babel to execute block at LOCATION in BUFFER.

Uses the current buffer if BUFFER is nil.

Returns the alist form of the JSON response.

The program’s JSON response is inserted into a buffer with the name
`md-babel-response-buffer-name', which see."
  (let ((buffer (or buffer (current-buffer)))
        (command (md-babel--make-execute-stdin-command location))
        (response-buffer (get-buffer-create
                          md-babel-response-buffer-name)))
    (with-current-buffer response-buffer
      (erase-buffer))
    (with-current-buffer buffer
      (call-shell-region (point-min) (point-max) command nil response-buffer))
    (let ((json-str (with-current-buffer response-buffer
                      (buffer-substring-no-properties (point-min) (point-max)))))
      (json-parse-string json-str :object-type 'alist))))

(defun md-babel--replace-response-buffer-content (string)
  "Replaces contents of buffer `md-babel-response-buffer-name' with STRING."
  (with-current-buffer (get-buffer-create md-babel-response-buffer-name)
    (erase-buffer)
    (insert string)))


;;;; File-based invocation

(defun md-babel--make-execute-file-command (file location)
  "Assembles the shell invocation to execute FILE at LOCATION."
  (let ((line (number-to-string (md-babel--source-location-line location)))
        (column (number-to-string (md-babel--source-location-column
                                   location))))
    (string-join
     (list md-babel-path
           "exec"
           "--file" (shell-quote-argument file)
           "--line" (shell-quote-argument line)
           "--column" (shell-quote-argument column))
     " ")))

(defun md-babel--execute-file (file location)
  "Instructs md-babel to execute block at LOCATION in FILE.

Returns the alist form of the JSON response.

The program’s JSON response is inserted into a buffer with the name
`md-babel-response-buffer-name', which see."
  (let* ((command (md-babel--make-execute-file-command file location))
         (json-str (with-temp-buffer
                     (shell-command command (current-buffer) nil)
                     (buffer-substring-no-properties (point-min) (point-max))
                     ))
         (result-buffer (get-buffer-create md-babel-response-buffer-name)))
    (with-current-buffer result-buffer
      (delete-region (point-min) (point-max))
      (insert json-str))
    (json-parse-string json-str :object-type 'alist)))


;;; User-facing commands

;;;###autoload
(defun md-babel-execute-block-at-point ()
  "Run block at `point' via the md-babel program and insert result.

The md-babel program produces a JSON response to describe the
transformation to the document that ought to be performed. The
response can be inspected in the buffer with name
`md-babel-response-buffer-name'. Evaluate this after loading the package
to get there:

    (get-buffer-create md-babel-response-buffer-name)

This command automatically interprets the response.
"
  (interactive)
  (when-let* ((location (md-babel--source-location-at-point))
              (response (md-babel--execute-buffer location)))
    (md-babel--interpret-response response)))

(provide 'md-babel)

;; Local Variables:
;; indent-tabs-mode: nil
;; coding: utf-8
;; End:
;;; md-babel.el ends here
