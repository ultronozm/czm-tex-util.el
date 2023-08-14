;;; czm-tex-util.el --- Utility functions for tex buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.1
;; URL: https://github.com/ultronozm/czm-tex-util.el
;; Package-Requires: ((emacs "26.1"))
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

;; This file contains utility functions for tex buffers.

;;; Code:

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

(defun czm-tex-util-get-label-number-helper (label aux-file)
  "Get the number of LABEL from the aux file AUX-FILE.
If LABEL is not found, return nil.  If LABEL is found, return the
number as a string."
  (when (file-exists-p aux-file)
    (with-current-buffer (find-file-noselect aux-file)
      (let ((result
	     (save-excursion
	       (goto-char (point-min))
	       (and
		(search-forward (concat "\\newlabel{" label "}") nil t)
		(search-forward "{{" nil t)
		(buffer-substring-no-properties
		 (point)
		 (progn (search-forward "}" nil t) (- (point) 1)))))))
	(kill-buffer)
	result))))


(defun czm-tex-util-environment-bounds ()
  "Returns a cons cell of the beginning and end of the current LaTeX environment region."
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


(provide 'czm-tex-util)
;;; czm-tex-util.el ends here
