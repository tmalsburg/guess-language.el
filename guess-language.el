;;; guess-language.el --- Automatically detect human language

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 2.0.0
;; Package-Requires: ((cl-lib "0.5"))

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

;; Just a proof of concept at this time.  Only supports English and
;; German but can easily be extended to handle other languages.

;; The detection algorithm is based on counts of character
;; trigrams.  The trigrams are copied from guess_language.py
;; (https://github.com/kent37/guess-language).

(defvar guess-language-languages '(en de))

(defun guess-language-load-trigrams ()
  (cl-loop
   for lang in guess-language-languages
   for trigrams = (with-temp-buffer
                    (insert-file-contents (symbol-name lang))
                    (split-string (buffer-string) "\n" t))
   collect (cons lang trigrams)))

(defun guess-language-compile-regexps ()
  (setq guess-language-regexps
        (cl-loop
         for lang in (guess-language-load-trigrams)
         for regexp = (mapconcat 'identity (cdr lang) "\\)\\|\\(")
         for regexp = (concat "\\(" regexp "\\)")
         collect (cons (car lang) regexp))))

(defun guess-language (beginning end)
  (let ((tally (cl-loop
                for lang in guess-language-regexps
                for regexp = (cdr lang)
                collect (cons (car lang) (how-many regexp beginning end)))))
    (print tally)
    (car (--max-by (> (cdr it) (cdr other)) tally))))

(defun guess-language-buffer ()
  (interactive)
  (print (guess-language (point-min) (point-max))))

(defun guess-language-paragraph ()
  (interactive)
  (let ((beginning (save-excursion (backward-paragraph) (point)))
        (end       (save-excursion (forward-paragraph) (point))))
    (print (guess-language beginning end))))

(defun guess-language-region ()
  (interactive)
  (print (guess-language (region-beginning) (region-end))))

(defun guess-language-autoset ()
  "Detects language of the current paragraph and sets things like
ispell dictionaries accordingly."
  (interactive)
  (pcase (guess-language-paragraph)
    ('en (progn
           (ispell-change-dictionary "en")
           (typo-change-language "English")))
    ('de (progn
           (ispell-change-dictionary "de")
           (typo-change-language "German"))))
  (flyspell-region (save-excursion (backward-paragraph) (point))
                   (save-excursion (forward-paragraph) (point))))

(provide 'guess-language)

;; Local Variables:
;; byte-compile-warnings: (not cl-functions obsolete)
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; guess-language.el ends here
