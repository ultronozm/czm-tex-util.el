;;; czm-tex-util.el --- Utility functions for tex buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-util.el
;; Package-Requires: ((emacs "26.1") (auctex "11.86.1"))
;; Keywords: tex

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package contains utility functions for tex buffers.

;;; Code:

(require 'tex)
(require 'latex)

(defvar czm-tex-util--cache (make-hash-table :test 'equal))

(defun czm-tex-util-update-cache (aux-file)
  "Update the cache for AUX-FILE."
  (when (file-exists-p aux-file)
    (with-current-buffer (find-file-noselect aux-file)
      ;; scan file to populate cache
      (let ((cache (make-hash-table :test 'equal))
            (pattern "\\newlabel{\\([^}]+\\)}{{\\([^}]+\\)}"))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (let ((label (match-string 1))
                  (number (match-string 2)))
              (puthash label number cache))))
        (puthash 'timestamp (current-time) cache)
        (puthash aux-file cache czm-tex-util--cache)
        (kill-buffer)
        cache))))

(defun czm-tex-util-get-label-number-helper (label aux-file)
  "Get the number of LABEL from the aux file AUX-FILE."
  (let ((cache (gethash aux-file czm-tex-util--cache)))
    (if (or (not cache)
            (time-less-p (gethash 'timestamp cache)
                         (nth 5 (file-attributes aux-file))))
        (setq cache (czm-tex-util-update-cache aux-file)))
    (when cache
      (gethash label cache))))

(defun czm-tex-util-get-label-number (label)
  "Get number of LABEL for current tex buffer.
If the buffer does not point to a file, or if the corresponding
aux file does not exist, or if the label cannot be found, then
return nil.  Otherwise, return the label number as a string.  If
the label is found in an external document, prefix the string
with \"X\"."
  (or
   (when-let* ((tex-file (buffer-file-name))
	       (aux-file (concat (file-name-sans-extension tex-file) ".aux")))
     (czm-tex-util-get-label-number-helper label aux-file))
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (let (found)
	 (while (and (null found)
		     (re-search-forward "\\\\externaldocument{\\([^}]+\\)}" nil t))
	   (let*
	       ((filename (concat (match-string 1) ".aux")))
	     (setq found (czm-tex-util-get-label-number-helper label filename))))
	 (when found
	   (concat "X" found)))))))

(defun czm-tex-util-environment-bounds ()
  "Return cons cell describing current LaTeX environment."
  (LaTeX-mark-environment)
  (let ((begin (region-beginning))
        (end (region-end)))
    (goto-char begin)
    (forward-line 1)
    (setq begin (point))
    (goto-char end)
    (forward-line -1)
    (deactivate-mark) ; deactivate the mark after the function has been called
    (cons begin (point))))

(defun czm-tex-util-get-bib-files ()
  "Get bib files for current buffer.
If the current buffer contains a \\bibliography command, return
a list containing the files referenced by that command.  Otherwise, return nil."
  (save-excursion
    (save-restriction
      (widen)
      (goto-char (point-min))
      (when (re-search-forward "\\\\bibliography{\\([^}]+\\)}" nil t)
        (let ((contents (match-string 1)))
          ;; contents is a comma-delimited list of files, which may
          ;; contain extra spaces.  Each file is either a relative or
          ;; absolute path to a .bib file.  The .bib extension is
          ;; optional.
          (mapcar
           (lambda (x)
             (let ((file (expand-file-name
                          (concat (file-name-sans-extension x) ".bib"))))
               (if (file-exists-p file)
                   file
                 (user-error "BibTeX file %s does not exist" file))))
           (split-string contents "[, ]+" t)))))))

(defun czm-tex-util-remove-braces-accents (input)
  "Remove braces and accepts from string INPUT.
In practice, INPUT is a BiBTeX author name or title.  The idea is
to make it easier to search for author names with accents.
Probably there's some standard library that does this, but I
couldn't quickly find it."
  (with-temp-buffer
    (insert input)
    (goto-char (point-min))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\{"
	      "{"
	      "}"
	      "\\'"
	      "\\`"
	      "\\^"
	      "\\\""
	      "\\~"))
	    nil t)
      (unless (save-match-data (texmathp))
	(replace-match "" t t)))
    (goto-char (point-min))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\l "
	      "\\l"))
	    nil t)
      (unless (texmathp)
	(replace-match "l")))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\cprime "
	      "\\cprime"))
	    nil t)
      (unless (texmathp)
	(replace-match "")))
    (while (re-search-forward
	    (regexp-opt
	     (list
	      "\\oe "
	      "\\oe"))
	    nil t)
      (unless (texmathp)
	(replace-match "oe")))
    (goto-char (point-min))
    (while (re-search-forward
	    "\\\\\\([a-zA-Z]\\)"
	    nil t)
      (unless (texmathp)
	(replace-match "l")))
    (while (re-search-forward
	    "ḡ"
	    nil t)
      (unless (texmathp)
	(replace-match "g")))
    (buffer-substring-no-properties (point-min) (point-max))))

(provide 'czm-tex-util)
;;; czm-tex-util.el ends here
