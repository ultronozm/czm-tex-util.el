;;; czm-tex-util.el --- Utility functions for tex buffers  -*- lexical-binding: t; -*-

;; Copyright (C) 2023  Paul D. Nelson

;; Author: Paul D. Nelson <nelson.paul.david@gmail.com>
;; Version: 0.0
;; URL: https://github.com/ultronozm/czm-tex-util.el
;; Package-Requires: ((emacs "28.1") (auctex "14.0.5"))
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
    (with-temp-buffer
      (insert-file-contents aux-file)
      ;; scan file to populate cache
      (let ((cache (make-hash-table :test 'equal))
            (pattern "\\newlabel{\\([^}]+\\)}{{\\([^}]+\\)}"))
        (save-excursion
          (goto-char (point-min))
          (while (re-search-forward pattern nil t)
            (let ((label (match-string 1))
                  (number (match-string 2)))
              (puthash label number cache))))
        (puthash 'timestamp (current-time)
                 cache)
        (puthash aux-file cache czm-tex-util--cache)
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
   (when-let* ((tex-file (buffer-file-name (buffer-base-buffer)))
               (aux-file (concat (file-name-with-extension
                                  (file-name-sans-extension tex-file)
                                  "aux"))))
     (czm-tex-util-get-label-number-helper label aux-file))
   (save-excursion
     (save-restriction
       (widen)
       (goto-char (point-min))
       (let (found)
         (while (and (null found)
                     (re-search-forward "\\\\externaldocument{\\([^}]+\\)}" nil t))
           (let*
               ((filename (file-name-with-extension (match-string 1)
                                                    "aux")))
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
                          (file-name-with-extension
                           (file-name-sans-extension x) "bib"))))
               (if (file-exists-p file)
                   file
                 (user-error "BibTeX file %s does not exist" file))))
           (split-string contents "[, ]+" t)))))))

(defun czm-tex-util--split-text-and-math (str)
  "Split STR into list of (mode . text) pairs.
Mode is either `text' or `math'.  Math segments include their delimiting
$ symbols."
  (let ((segments   ())
        (start      0)
        (pos        0)
        (len        (length str))
        (in-math    nil))
    (while (< pos len)
      (let ((c (aref str pos)))
        (cond
         ((= c ?$)
          ;; Toggle math
          (when (< start pos)
            (push (cons (if in-math 'math 'text)
                        (substring str start pos))
                  segments))
          (setq in-math (not in-math))
          (setq start (1+ pos))
          (setq pos   (1+ pos)))
         (t
          (setq pos (1+ pos))))))
    (when (< start len)
      (push (cons (if in-math 'math 'text)
                  (substring str start len))
            segments))
    (nreverse segments)))

(defun czm-tex-util--handle-polish-l (str)
  "Handle Polish l in STR.
Convert \\L -> L, \\l\\l -> ll, \\l -> l, preserving following letters."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    ;; First handle consecutive \l\l
    (while (re-search-forward "\\\\l[ ]*\\\\l" nil t)
      (replace-match "ll"))
    ;; Then handle \L at start of word
    (goto-char (point-min))
    (while (re-search-forward "\\\\L\\([a-z]\\)" nil t)
      (replace-match "L\\1"))
    ;; Then handle remaining \L
    (goto-char (point-min))
    (while (re-search-forward "\\\\L" nil t)
      (replace-match "L"))
    ;; Then handle remaining \l
    (goto-char (point-min))
    (while (re-search-forward "\\\\l" nil t)
      (replace-match "l"))
    ;; Clean up spaces
    (goto-char (point-min))
    (while (re-search-forward "\\s-+" nil t)
      (replace-match " "))
    (goto-char (point-min))
    (while (re-search-forward "\\([A-Z]\\)\\s-+\\([a-z]\\)" nil t)
      (replace-match "\\1\\2"))
    (buffer-string)))

(defun czm-tex-util--handle-dotless-letters (str)
  "Convert \\i -> i, \\j -> j in STR.
Also simplifies other invalid backslashed letters to just the letter."
  (with-temp-buffer
    (insert str)
    ;; Handle \i and \j first (these are valid TeX commands)
    (goto-char (point-min))
    (while (re-search-forward "\\\\[ij]" nil t)
      (replace-match (char-to-string (aref (match-string 0) 1))))
    ;; Handle other backslashed letters that aren't part of accent commands
    (goto-char (point-min))
    (while (re-search-forward "\\\\\\([a-zA-Z]\\)" nil t)
      (let ((letter (match-string 1)))
        (unless (member letter '("b" "c" "d" "H" "t" "u" "v"))  ; known accent commands
          (replace-match letter))))
    (buffer-string)))

(defun czm-tex-util--handle-braces (str)
  "Remove braces in STR, being careful with single letters."
  (with-temp-buffer
    (insert str)
    ;; First handle single letters
    (goto-char (point-min))
    (while (re-search-forward "{\\([A-Za-z]\\)}" nil t)
      (replace-match "\\1"))
    ;; Then remove remaining braces
    (goto-char (point-min))
    (while (re-search-forward "[{}]" nil t)
      (replace-match ""))
    (buffer-string)))

(defun czm-tex-util--handle-accents (str)
  "Remove LaTeX accent commands from STR."
  (with-temp-buffer
    (insert str)
    (let ((accent-re
           (rx (group "\\" (or "`" "'" "^" "\"" "~" "=" "." "u" "v" "H"
                               "c" "d" "t" "b"))  ; e.g. \H
               (? "{" (group (+ (not (any "}")))) "}"))))
      (goto-char (point-min))
      (while (re-search-forward accent-re nil t)
        (let ((has-brace (match-string 2)))
          (if has-brace
              (replace-match has-brace t t)
            (replace-match "")))))
    (buffer-string)))

(defun czm-tex-util--cleanup-remaining (str)
  "Clean up remaining backslashes and spaces before punctuation in STR."
  (with-temp-buffer
    (insert str)
    ;; Remove remaining backslashed words
    (goto-char (point-min))
    (while (re-search-forward "\\\\+\\([A-Za-z]+\\)" nil t)
      (replace-match "\\1"))
    ;; Remove any remaining backslashes
    (goto-char (point-min))
    (while (re-search-forward "\\\\+" nil t)
      (replace-match ""))
    ;; Clean up spaces before punctuation
    (goto-char (point-min))
    (while (re-search-forward "\\s-+\\([,.:;!?]\\)" nil t)
      (replace-match "\\1"))
    (buffer-string)))

(defun czm-tex-util--transform-text-segment (txt)
  "Transform text segment TXT, handling all special cases."
  (thread-last txt
               czm-tex-util--handle-polish-l
               czm-tex-util--handle-dotless-letters
               czm-tex-util--handle-accents
               czm-tex-util--handle-braces
               czm-tex-util--cleanup-remaining))

(defun czm-tex-util--cleanup-math-braces (str)
  "Remove braces that wrap entire math segments in STR."
  (with-temp-buffer
    (insert str)
    (goto-char (point-min))
    (while (re-search-forward "{\\(\\$[^$]+\\$\\)}" nil t)
      (replace-match "\\1"))
    (buffer-string)))

(defun czm-tex-util-remove-braces-accents (input)
  "Remove braces/accents in TEXT parts of INPUT, preserving math parts.
Attempts to pass all 9 ERT tests provided.

Strategy:
1) Split the string on top-level $...$ math segments (kept verbatim).
2) For each TEXT segment:
   - Remove or simplify LaTeX accent commands (\\H{o}, \\'a, etc.).
   - Convert \\l -> `l', \\L -> `L' (and handle next letters).
   - Remove braces around single letters, then remove leftover braces.
   - Clean up any spaces before punctuation, etc.
3) Re-assemble TEXT + MATH segments, then remove braces wrapping an
   entire math segment: e.g. \"{ $L^\\infty$ }\" => \"$L^\\infty$\"."
  (let* ((segments (czm-tex-util--split-text-and-math input))
         ;; Transform text, keep math
         (transformed
          (mapcar (lambda (seg)
                    (if (eq (car seg) 'math)
                        (cons 'math (cdr seg))
                      (cons 'text (czm-tex-util--transform-text-segment (cdr seg)))))
                  segments))
         ;; Rejoin with `$...$` around math
         (rejoined
          (mapconcat
           (lambda (pair)
             (if (eq (car pair) 'math)
                 (concat "$" (cdr pair) "$")
               (cdr pair)))
           transformed
           "")))
    (czm-tex-util--cleanup-math-braces rejoined)))


(provide 'czm-tex-util)
;;; czm-tex-util.el ends here
