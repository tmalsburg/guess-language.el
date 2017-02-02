;;; guess-language.el --- Automatically detect human language

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (typo "1.1"))

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

;; Guess-language is a buffer-local minor mode.  It guesses the
;; language of the current paragraph when flyspell detects an
;; incorrect word and changes ispell's dictionary and typo-mode
;; accordingly.  If the language settings change, flyspell is rerun
;; but only on the current paragraph.  Guess-language thus supports
;; documents using multiple languages.  If the paragraph is shorter
;; than some user-defined value, none of the above happens because
;; there is likely not enough text to guess the language correctly.
;;
;; The detection algorithm is based on counts of character
;; trigrams.  At this time, supported languages are Czech, Dansk,
;; Dutch, English, Finnish, French, German, Italian, Norwegian,
;; Polish, Portuguese, Russian, Slovak, Slovenian, Swedish.  Adding
;; further languages is very easy and this package already contains
;; language statistics for 49 additional languages.

;; See here for more details:
;; https://github.com/tmalsburg/guess-language.el

;;; Code:

(require 'cl-lib)
(require 'typo)
(require 'find-func)
(require 'ispell)
(require 'flyspell)

(defcustom guess-language-languages '(en de fr)
  "List of symbols that identify the languages that should be
considered when guessing language.  Currently supported
languages are:

  cs: Czech
  da: Dansk
  nl: Dutch
  en: English
  fi: Finnish
  fr: French
  de: German
  it: Italian
  nb: Norwegian
  pl: Polish
  pt: Portuguese
  ru: Russian
  sk: Slovak
  sl: Slovenian
  sv: Swedish "
  :type '(repeat symbol)
  :group 'guess-language)

(defcustom guess-language-min-paragraph-length 40
  "When a paragraph is shorter than this value (in characters),
guess-language doesn't do anything because there is likely too
little material to reliably guess the language."
  :type 'integer
  :group 'guess-language)

(defvar guess-language-regexps nil
  "The regular expressions that are used to count trigrams.")

(defvar guess-language-langcodes
  '(
    ;; Languages with ispell and typo support:
    (cs . ("czech" "Czech"))
    (de . ("de" "German"))
    (en . ("en" "English"))
    (fi . ("finnish" "Finnish"))
    (it . ("italiano" "Italian"))
    (fr . ("francais" "French"))
    (ru . ("russian" "Russian"))
    ;; Languages with ispell but no typo support:
    (da . ("dansk" nil))
    (nl . ("nederlands" nil))
    (nb . ("norsk" nil))
    (pl . ("polish" nil))
    (pt . ("portuguese" nil))
    (sk . ("slovak" nil))
    (sl . ("slovenian" nil))
    (sv . ("svenska" nil)))
  "Language codes for ispell and typo-mode.")

(defun guess-language-load-trigrams ()
  (cl-loop
   for lang in guess-language-languages
   for fname = (expand-file-name (symbol-name lang)
                                 (expand-file-name "trigrams"
                                                   (file-name-directory
                                                    (or load-file-name buffer-file-name))))
   for trigrams = (with-temp-buffer
                    (insert-file-contents fname)
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
  (unless guess-language-regexps
    (guess-language-compile-regexps))
  (when (cl-set-exclusive-or guess-language-languages (mapcar #'car guess-language-regexps))
    (guess-language-compile-regexps))
  (let ((tally (cl-loop
                for lang in guess-language-regexps
                for regexp = (cdr lang)
                collect (cons (car lang) (how-many regexp beginning end)))))
    (car (cl-reduce (lambda (x y) (if (> (cdr x) (cdr y)) x y)) tally))))

(defun guess-language-buffer ()
  (guess-language (point-min) (point-max)))

(defun guess-language-paragraph ()
  (let ((beginning (save-excursion (backward-paragraph) (point)))
        (end       (save-excursion (forward-paragraph) (point))))
    (guess-language beginning end)))

(defun guess-language-region ()
  (guess-language (region-beginning) (region-end)))

(defun guess-language-autoset ()
  "Detects language of the current paragraph and changes updates
ispell and typo mode accordingly.  If typo doesn't support the
language, we leave it alone."
  (interactive)
  (let* ((lang (guess-language-paragraph))
         (codes (cdr (assoc lang guess-language-langcodes))))
    (ispell-change-dictionary (car codes))
    (when (cadr codes)
      (typo-change-language (cadr codes)))))

(defun guess-language-autoset-and-spellcheck-maybe (beginning end doublon)
  "Runs `guess-language-autoset' and then the flyspell on the
current paragraph."
  (let ((old-dictionary ispell-local-dictionary)
        (beginning (save-excursion (backward-paragraph) (point)))
        (end       (save-excursion (forward-paragraph) (point))))
    (when (> (- end beginning) guess-language-min-paragraph-length)
      (guess-language-autoset)
      (unless (string= old-dictionary ispell-local-dictionary)
        (remove-hook 'flyspell-incorrect-hook #'guess-language-autoset-and-spellcheck-maybe)
        (let ((flyspell-issue-welcome-flag nil)
              (flyspell-issue-message-flag nil))
          (flyspell-region (save-excursion (backward-paragraph) (point))
                           (save-excursion (forward-paragraph) (point)))
          (add-hook 'flyspell-incorrect-hook #'guess-language-autoset-and-spellcheck-maybe))))))

(define-minor-mode guess-language-mode
  "Toggle guess-language mode.
Interactively with no argument, this command toggles the mode.
A positive prefix argument enables the mode, any other prefix
argument disables it.  From Lisp, argument omitted or nil enables
the mode, `toggle' toggles the state.

Guess-language is a buffer-local minor mode.  It guesses the
language of the current paragraph when flyspell detects an
incorrect word and changes ispell's dictionary and typo-mode
accordingly.  If the language settings change, flyspell is rerun
on the current paragraph.  If the paragraph is shorter than
`guess-language-min-paragraph-length', none of the above happens
because there is likely not enough text to guess the language
correctly."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Guess-lang"
  :global nil
  :group 'guess-language
  (if guess-language-mode
      (add-hook 'flyspell-incorrect-hook #'guess-language-autoset-and-spellcheck-maybe nil t)
    (remove-hook 'flyspell-incorrect-hook #'guess-language-autoset-and-spellcheck-maybe t)))

(provide 'guess-language)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; guess-language.el ends here
