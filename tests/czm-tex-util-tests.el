;;; czm-tex-util-tests.el --- tests for czm-tex-util  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Paul Nelson

;; Author: Paul Nelson <ultrono@gmail.com>
;; Keywords: 

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

;;

;;; Code:

(require 'ert)

(ert-deftest czm-tex-util-remove-braces-accents-test-schubert ()
  "Test GL math mode preservation."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "Schubert cells and Whittaker functionals for $\\text{GL}(n,\\mathbb{R})$ part II: Existence via integration by parts")
                   "Schubert cells and Whittaker functionals for $\\text{GL}(n,\\mathbb{R})$ part II: Existence via integration by parts")))

(ert-deftest czm-tex-util-remove-braces-accents-test-modular ()
  "Test mathrm GL preservation."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "Modular representations of $\\mathrm{GL}_2({\\mathbb F}_q)$ using calculus")
                   "Modular representations of $\\mathrm{GL}_2({\\mathbb F}_q)$ using calculus")))

(ert-deftest czm-tex-util-remove-braces-accents-test-quantum ()
  "Test PGL math mode preservation."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "Quantum ergodicity on the Bruhat-Tits building for $\\text{PGL}(3, F)$ in the Benjamini-Schramm limit")
                   "Quantum ergodicity on the Bruhat-Tits building for $\\text{PGL}(3, F)$ in the Benjamini-Schramm limit")))

(ert-deftest czm-tex-util-remove-braces-accents-test-spectral ()
  "Test braces around math mode."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "Spectral aspect subconvex bounds for {$U_{n+1} \\times U_n$}")
                   "Spectral aspect subconvex bounds for $U_{n+1} \\times U_n$")))

(ert-deftest czm-tex-util-remove-braces-accents-test-whittaker ()
  "Test capitalization braces and math mode."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "The {W}hittaker period formula on metaplectic {$\\rm SL_2$}")
                   "The Whittaker period formula on metaplectic $\\rm SL_2$")))

(ert-deftest czm-tex-util-remove-braces-accents-test-linfty ()
  "Test braces around math at start."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "{$L^\\infty$} norms of eigenfunctions")
                   "$L^\\infty$ norms of eigenfunctions")))

(ert-deftest czm-tex-util-remove-braces-accents-test-voronoi ()
  "Test special character handling."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "On the {V}orono\\u{\\i} formula for {${\\rm GL}(n)$}")
                   "On the Voronoi formula for ${\\rm GL}(n)$")))

(ert-deftest czm-tex-util-remove-braces-accents-test-radziwill ()
  "Test Polish special characters."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "Radziwi\\l \\l , Maksym and Soundararajan, K.")
                   "Radziwill, Maksym and Soundararajan, K.")))

(ert-deftest czm-tex-util-remove-braces-accents-test-erdos ()
  "Test multiple accent types."
  (should (string= (czm-tex-util-remove-braces-accents 
                    "Erd\\H{o}s, P\\'al and \\L uczak, Tomasz and R\\'enyi, Alfr\\'ed")
                   "Erdos, Pal and Luczak, Tomasz and Renyi, Alfred")))


(ert-deftest czm-tex-util--split-text-and-math-test ()
  "Test splitting into text and math segments."
  ;; Simple cases
  (should (equal (czm-tex-util--split-text-and-math "Plain text")
                 '((text . "Plain text"))))
  (should (equal (czm-tex-util--split-text-and-math "$x^2$")
                 '((math . "x^2"))))
  ;; Mixed cases
  (should (equal (czm-tex-util--split-text-and-math "Start $x^2$ middle $y^2$ end")
                 '((text . "Start ")
                   (math . "x^2")
                   (text . " middle ")
                   (math . "y^2")
                   (text . " end"))))
  ;; Edge cases
  (should (equal (czm-tex-util--split-text-and-math "$x^2$ end")
                 '((math . "x^2")
                   (text . " end"))))
  (should (equal (czm-tex-util--split-text-and-math "start $x^2$")
                 '((text . "start ")
                   (math . "x^2")))))

(ert-deftest czm-tex-util--handle-polish-l-test ()
  "Test handling of Polish l characters."
  ;; Basic cases
  (should (string= (czm-tex-util--handle-polish-l "\\l") "l"))
  (should (string= (czm-tex-util--handle-polish-l "\\L") "L"))
  ;; Double l
  (should (string= (czm-tex-util--handle-polish-l "\\l\\l") "ll"))
  (should (string= (czm-tex-util--handle-polish-l "\\l \\l") "ll"))
  ;; Real names
  (should (string= (czm-tex-util--handle-polish-l "\\L uczak") "Luczak"))
  (should (string= (czm-tex-util--handle-polish-l "Radziwi\\l\\l") "Radziwill"))
  ;; Mixed cases
  (should (string= (czm-tex-util--handle-polish-l "\\L uczak and \\l\\l") "Luczak and ll")))

(ert-deftest czm-tex-util--handle-dotless-letters-test ()
  "Test handling of dotless i and j."
  ;; Basic dotless i/j cases
  (should (string= (czm-tex-util--handle-dotless-letters "\\i") "i"))
  (should (string= (czm-tex-util--handle-dotless-letters "\\j") "j"))
  (should (string= (czm-tex-util--handle-dotless-letters "\\i\\j") "ij"))
  (should (string= (czm-tex-util--handle-dotless-letters "\\i test \\j") "i test j"))
  ;; Cases with accent commands (should be preserved)
  (should (string= (czm-tex-util--handle-dotless-letters "\\a\\b\\i\\c") "a\\bi\\c"))  ; \b is accent
  (should (string= (czm-tex-util--handle-dotless-letters "\\x\\y\\i\\u") "xyi\\u")))

(ert-deftest czm-tex-util--handle-braces-test ()
  "Test handling of braced content."
  ;; Single letters
  (should (string= (czm-tex-util--handle-braces "{W}") "W"))
  (should (string= (czm-tex-util--handle-braces "{a}") "a"))
  ;; Multiple braces
  (should (string= (czm-tex-util--handle-braces "{{W}}") "W"))
  ;; Mixed content
  (should (string= (czm-tex-util--handle-braces "{W}hittaker") "Whittaker"))
  (should (string= (czm-tex-util--handle-braces "The {W}hittaker") "The Whittaker"))
  ;; Non-letter content
  (should (string= (czm-tex-util--handle-braces "{123}") "123"))
  ;; Multiple instances
  (should (string= (czm-tex-util--handle-braces "{A} and {B}") "A and B")))

(ert-deftest czm-tex-util--handle-accents-test ()
  "Test handling of LaTeX accent commands."
  ;; Basic accents with braces
  (should (string= (czm-tex-util--handle-accents "\\H{o}") "o"))
  (should (string= (czm-tex-util--handle-accents "\\'e") "e"))
  (should (string= (czm-tex-util--handle-accents "\\^{i}") "i"))
  ;; Multiple accents
  (should (string= (czm-tex-util--handle-accents "\\H{o}\\'{e}") "oe"))
  ;; Real examples
  (should (string= (czm-tex-util--handle-accents "Erd\\H{o}s") "Erdos"))
  (should (string= (czm-tex-util--handle-accents "P\\'al") "Pal"))
  ;; Complex cases
  (should (string= (czm-tex-util--handle-accents "R\\'enyi, Alfr\\'ed") "Renyi, Alfred")))

(ert-deftest czm-tex-util--cleanup-remaining-test ()
  "Test cleanup of remaining backslashes and spacing."
  ;; Backslashed words
  (should (string= (czm-tex-util--cleanup-remaining "\\text") "text"))
  (should (string= (czm-tex-util--cleanup-remaining "\\TeX") "TeX"))
  ;; Spaces before punctuation
  (should (string= (czm-tex-util--cleanup-remaining "hello , world") "hello, world"))
  (should (string= (czm-tex-util--cleanup-remaining "hello . World") "hello. World"))
  ;; Multiple cases
  (should (string= (czm-tex-util--cleanup-remaining "A \\cmd , B . C")
                   "A cmd, B. C")))

(ert-deftest czm-tex-util--cleanup-math-braces-test ()
  "Test cleanup of braces around math mode."
  ;; Simple case
  (should (string= (czm-tex-util--cleanup-math-braces "{$x^2$}")
                   "$x^2$"))
  ;; Multiple instances
  (should (string= (czm-tex-util--cleanup-math-braces "{$x^2$} and {$y^2$}")
                   "$x^2$ and $y^2$"))
  ;; Real examples
  (should (string= (czm-tex-util--cleanup-math-braces "{$L^\\infty$} norms")
                   "$L^\\infty$ norms"))
  ;; Should not affect other braces
  (should (string= (czm-tex-util--cleanup-math-braces "{text} {$math$}")
                   "{text} $math$")))

(ert-deftest czm-tex-util--transform-text-segment-test ()
  "Test complete text segment transformation."
  ;; Complex real-world examples
  (should (string= (czm-tex-util--transform-text-segment "Erd\\H{o}s, P\\'al")
                   "Erdos, Pal"))
  (should (string= (czm-tex-util--transform-text-segment "\\L uczak")
                   "Luczak"))
  (should (string= (czm-tex-util--transform-text-segment "The {W}hittaker")
                   "The Whittaker"))
  (should (string= (czm-tex-util--transform-text-segment "Radziwi\\l \\l")
                   "Radziwill")))

(when nil
  (ert-run-tests-interactively "^czm-tex-util-"))


(provide 'czm-tex-util-tests)
;;; czm-tex-util-tests.el ends here
