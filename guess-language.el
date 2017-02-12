;;; guess-language.el --- Robust automatic language detection

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (emacs "24") (typo "1.1"))
;; URL: https://github.com/tmalsburg/guess-language.el

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
;; incorrect word and changes Ispell's dictionary and typo-mode (if
;; present) accordingly.  If the language settings change, flyspell is
;; rerun but only on the current paragraph.  Guess-language thus
;; supports documents using multiple languages.  If the paragraph is
;; shorter than some user-defined value, none of the above happens
;; because there is likely not enough text to guess the language
;; correctly.
;;
;; The detection algorithm is based on counts of character
;; trigrams.  At this time, supported languages are Arabic, Czech,
;; Danish, Dutch, English, Finnish, French, German, Italian,
;; Norwegian, Polish, Portuguese, Russian, Slovak, Slovenian, Spanish,
;; Swedish.  Adding further languages is very easy and this package
;; already contains language statistics for 49 additional languages.

;;; Code:

(require 'cl-lib)
(require 'find-func)
(require 'ispell)
(require 'flyspell)

(defgroup guess-language nil
  "Minor mode that automatically guesses the language being
typed.  Automatically switches things like spell-checker
dictionary, input methods, etc."
  :group 'completion)

(defcustom guess-language-languages '(en de fr)
  "List of languages that should be considered.

Uses ISO 639-1 identifiers.  Currently supported languages are:
Arabic (ar),  Czech (cs),  Danish (da),  Dutch (nl),  English (en),
Finnish (fi),  French (fr),  German (de),  Italian (it),
Norwegian (nb),  Polish (pl),  Portuguese (pt),  Russian (ru),
Slovak (sk),  Slovenian (sl),  Spanish (es),  Swedish (sv)"
  :type '(repeat symbol)
  :group 'guess-language)

(defcustom guess-language-min-paragraph-length 40
  "Minimum number of characters in paragraph.

When a paragraph is shorter than this value (in characters),
guess-language doesn't do anything because there is likely too
little material to reliably guess the language."
  :type 'integer
  :group 'guess-language)

(defvar guess-language-regexps nil
  "The regular expressions that are used to count trigrams.")

(defcustom guess-language-langcodes
  '((ar . ("ar"         nil))
    (cs . ("czech"      "Czech"))
    (da . ("dansk"      nil))
    (de . ("de"         "German"))
    (en . ("en"         "English"))
    (es . ("spanish"    nil))
    (fi . ("finnish"    "Finnish"))
    (fr . ("francais"   "French"))
    (it . ("italiano"   "Italian"))
    (nb . ("norsk"      nil))
    (nl . ("nederlands" nil))
    (pl . ("polish"     nil))
    (pt . ("portuguese" nil))
    (ru . ("russian"    "Russian"))
    (sk . ("slovak"     nil))
    (sl . ("slovenian"  nil))
    (sv . ("svenska"    nil)))
  "Language codes for spell-checker and typo-mode.

The key is a symbol specifying the ISO 639-1 code of the
language.  The values is a list with two elements.  The first is
the name of the dictionary that should be used by the
spell-checker (e.g., what you would enter when setting the
language with `ispell-change-dictionary').  The second element is
the name of the language setting that should be used with
typo-mode.  If a language is not supported by typo-mode, that
value is nil."
  :type '(alist :key-type symbol :value-type list)
  :group 'guess-language)

(defcustom guess-language-after-detection-functions '()
  "Hook run when a new language is detected.

This hook is abnormal in that its functions take arguments,
namely a symbol indicating the language that was detected and the
beginning and end of the region in which the language was
detected."
  :type 'hook
  :group 'guess-language)

(defvar guess-language-current-language nil
  "The language detected when `guess-language' was last executed.")

(defun guess-language-load-trigrams ()
  "Load language statistics."
  (cl-loop
   for lang in guess-language-languages
   for basedir = (file-name-directory (find-library-name "guess-language"))
   for fname = (let ((dir1 (expand-file-name (symbol-name lang) basedir))
                     (dir2 (expand-file-name (symbol-name lang) (expand-file-name "trigrams" basedir))))
                 (if (file-exists-p dir1) dir1 dir2))
   for trigrams = (with-temp-buffer
                    (insert-file-contents fname)
                    (split-string (buffer-string) "\n" t))
   collect (cons lang trigrams)))

(defun guess-language-compile-regexps ()
  "Compile regular expressions used for guessing language."
  (setq guess-language-regexps
        (cl-loop
         for lang in (guess-language-load-trigrams)
         for regexp = (mapconcat 'identity (cdr lang) "\\)\\|\\(")
         for regexp = (concat "\\(" regexp "\\)")
         collect (cons (car lang) regexp))))

(defun guess-language-region (beginning end)
  "Guess language in the specified region.

Region starts at BEGINNING and ends at END."
  (interactive "*r")
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
  "Guess the language of the buffer."
  (guess-language-region (point-min) (point-max)))

(defun guess-language-paragraph ()
  "Guess the language of the current paragraph."
  (let ((beginning (save-excursion (backward-paragraph) (point)))
        (end       (save-excursion (forward-paragraph) (point))))
    (guess-language-region beginning end)))

(defun guess-language ()
  "Guess language of the current paragraph.

Calls the functions in
`guess-language-after-detection-functions`.  These functions may
switch the dictionary of the spell checker and do other useful
things like changing the keyboard layout or input method."
  (interactive)
  (let ((beginning (save-excursion (backward-paragraph) (point)))
        (end       (save-excursion (forward-paragraph)  (point))))
    (when (> (- end beginning) guess-language-min-paragraph-length)
      (let ((lang (guess-language-region beginning end)))
        (run-hook-with-args 'guess-language-after-detection-functions lang beginning end)
        (setq guess-language-current-language lang)))))

(defun guess-language-function (beginning end doublon)
  "Wrapper for `guess-language' because `flyspell-incorrect-hook'
provides three arguments that we don't need."
  (guess-language))

(defun guess-language-switch-flyspell-function (lang beginning end)
  "Switch the Flyspell dictionary spell-checks current paragraph.

This is only done if the new language is actually different from
the previous language.  Otherwise, nothing happens.

LANG is the ISO 639-1 code of the language (as a
symbol).  BEGINNING and END are the endpoints of the region in
which LANG was detected."
  (let* ((old-dictionary (cadr (assq guess-language-current-language guess-language-langcodes)))
         (new-dictionary (cadr (assq lang guess-language-langcodes))))
    (unless (string= old-dictionary new-dictionary)
      (ispell-change-dictionary new-dictionary)
      (let ((flyspell-issue-welcome-flag nil)
            (flyspell-issue-message-flag nil)
            (flyspell-incorrect-hook nil)
            (flyspell-large-region 1))
        (flyspell-region beginning end)))))

(defun guess-language-switch-typo-mode-function (lang beginning end)
  "Switch the language used by typo-mode.

LANG is the ISO 639-1 code of the language (as a
symbol).  BEGINNING and END are the endpoints of the region in
which LANG was detected."
  (let* ((typo-lang (cl-caddr (assq lang guess-language-langcodes))))
    (when typo-lang
      (typo-change-language typo-lang))))

;;;###autoload
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
  :lighter (:eval (format " (%s)" (or ispell-local-dictionary "default")))
  :global nil
  :group 'guess-language
  (if guess-language-mode
      (add-hook 'flyspell-incorrect-hook #'guess-language-function nil t)
    (remove-hook 'flyspell-incorrect-hook #'guess-language-function t)))


;;;###autoload
(add-hook 'guess-language-after-detection-functions 'guess-language-switch-flyspell-function)

(provide 'guess-language)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; guess-language.el ends here
