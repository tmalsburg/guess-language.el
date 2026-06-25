;;; guess-language.el --- Robust automatic language detection  -*- lexical-binding:t -*-

;; Copyright (C) 2019  Free Software Foundation, Inc.

;; Author: Titus von der Malsburg <malsburg@posteo.de>
;; Maintainer: Titus von der Malsburg <malsburg@posteo.de>
;; Description: Robust automatic language detection
;; Keywords: wp
;; Version: 0.0.1
;; Package-Requires: ((cl-lib "0.5") (emacs "24"))
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
;; supports documents using multiple languages.
;;
;; If the paragraph is shorter than some user-defined value, none of
;; the above happens because there is likely not enough text to guess
;; the language correctly.
;;
;; Custom functions can be triggered when a new language is detected
;; such that users can do things like changing the input method when
;; needed.
;;
;; The detection algorithm is based on counts of character trigrams. At this
;; time, supported languages are Arabic, Czech, Danish, Dutch, English,
;; Esperanto, Finnish, French, German, Italian, Norwegian, Polish, Portuguese,
;; Russian, Serbian, Slovak, Slovenian, Spanish, Swedish and Vietnamese. Adding
;; further languages is very easy and this package already contains language
;; statistics for 49 additional languages.

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

Uses ISO 639-1 identifiers. Currently supported languages are:
Arabic (ar), Czech (cs), Danish (da), Dutch (nl), English (en),
Esperanto (eo), Finnish (fi), French (fr), German (de),
Italian (it), Norwegian (nb), Polish (pl), Portuguese (pt),
Russian (ru), Slovak (sk), Slovenian (sl), Spanish (es),
Swedish (sv) and Vietnamese (vi)."
  :type '(repeat symbol))

(defcustom guess-language-min-paragraph-length 40
  "Minimum number of characters in paragraph.

When a paragraph is shorter than this value (in characters),
guess-language doesn't do anything because there is likely too
little material to reliably guess the language."
  :type 'integer)

(defvar guess-language--regexps nil
  "The regular expressions that are used to count trigrams.")

(defcustom guess-language-langcodes
  '((ar     . ("ar"         nil         "اَلْعَرَبِيَّةُ" "Arabic"))
    (cs     . ("czech"      "Czech"     "🇨🇿"   "Czech"))
    (da     . ("dansk"      nil         "🇩🇰"   "Danish"))
    (de     . ("de"         "German"    "🇩🇪"   "German"))
    (en     . ("en"         "English"   "🇬🇧"   "English"))
    (eo     . ("eo"         "Esperanto" "🟩"   "Esperanto"))
    (es     . ("spanish"    nil         "🇪🇸"   "Spanish"))
    (fi     . ("finnish"    "Finnish"   "🇫🇮"   "Finnish"))
    (fr     . ("francais"   "French"    "🇫🇷"   "French"))
    (it     . ("italiano"   "Italian"   "🇮🇹"   "Italian"))
    (nb     . ("norsk"      nil         "🇳🇴"   "Norsk"))
    (nl     . ("nederlands" nil         "🇳🇱"   "Dutch"))
    (pl     . ("polish"     "Polish"    "🇵🇱"   "Polish"))
    (pt     . ("portuguese" nil         "🇵🇹"   "Portuguese"))
    (ru     . ("russian"    "Russian"   "🇷🇺"   "Russian"))
    (sk     . ("slovak"     nil         "🇸🇰"   "Slovak"))
    (sl     . ("slovenian"  nil         "🇸🇮"   "Slovenian"))
    (sr     . ("serbian"    "Serbian"   "🇷🇸"   "Serbian"))
    (sr_LAT . ("sr-lat"     "Serbian"   "🇷🇸"   "Serbian"))
    (sv     . ("svenska"    "Swedish"   "🇸🇪"   "Swedish"))
    (vi     . ("viet"       nil         "🇻🇳"   "Vietnamese")))
  "Language codes for spell-checker and typo-mode.

The key is a symbol specifying the ISO 639-1 code of the
language.  The values is a list with four elements.  The first is
the name of the dictionary that should be used by the
spell-checker (e.g., what you would enter when setting the
language with `ispell-change-dictionary').  The second element is
the name of the language setting that should be used with
typo-mode.  If a language is not supported by typo-mode, that
value is nil.  The third element is a string for displaying the
current language in the mode line.  This could be text or a
Unicode flag symbol (displayed as color emoji starting from Emacs
28.1).  The last element is the name of the language for display
in the mini buffer."
  :type '(alist :key-type symbol
                :value-type (list string
                                  (choice string (const nil))
                                  string
                                  string)))

(defcustom guess-language-idle-delay 0.2
  "If non-nil, idle delay in seconds before guessing the language of the current paragraph."
  :type 'float)

(defcustom guess-language-after-detection-functions (list #'guess-language-switch-flyspell-function
                                                          #'guess-language-switch-typo-mode-function)
  "Hook run when a new language is detected.

This hook is abnormal in that its functions take arguments,
namely a symbol indicating the language that was detected and the
beginning and end of the region in which the language was
detected."
  :type 'hook)

(defcustom guess-language-trigrams-directory (file-name-directory (find-library-name "guess-language"))
  "Directory where trigrams are stored.

By default it's the same directory where this module is installed."
  :type '(file :must-match t))

(defvar guess-language-current-language nil
  "The language detected when `guess-language' was last executed.

Uses ISO 639-1 to identify languages.")
(make-variable-buffer-local 'guess-language-current-language)

(defvar-local guess-language--post-command-h #'ignore
  "Function called by `guess-language--post-command-h'.")

(defun guess-language-load-trigrams ()
  "Load language statistics."
  (cl-loop
   for lang in guess-language-languages
   for fname = (let ((dir1 (expand-file-name (symbol-name lang) guess-language-trigrams-directory))
                     (dir2 (expand-file-name (symbol-name lang) (expand-file-name "trigrams" guess-language-trigrams-directory))))
                 (if (file-exists-p dir1) dir1 dir2))
   for trigrams = (with-temp-buffer
                    (insert-file-contents fname)
                    (split-string (buffer-string) "\n" t))
   collect (cons lang trigrams)))

(defun guess-language-compile-regexps ()
  "Compile regular expressions used for guessing language."
  (setq guess-language--regexps
        (cl-loop
         for (lang . regexps) in (guess-language-load-trigrams)
         collect (cons lang (regexp-opt regexps)))))

(defun guess-language-backward-paragraph ()
  "Uses whatever method for moving to the previous paragraph is
most appropriate given the buffer mode."
  (if (derived-mode-p 'org-mode)
      ;; When in list, go to the beginning of the top-level list:
      (if (org-in-item-p)
          (org-beginning-of-item-list)
        (org-backward-paragraph))
    (backward-paragraph)
    (when (looking-at-p "[[:space:]]")
      (forward-whitespace 1))))

(defun guess-language-forward-paragraph ()
  "Uses whatever method for moving to the next paragraph is
most appropriate given the buffer mode."
  (if (derived-mode-p 'org-mode)
      (if (org-in-item-p)
          (org-end-of-item-list)
        (org-forward-paragraph))
    (forward-paragraph)))

(defun guess-language-region (beginning end)
  "Guess language in the specified region.

Region starts at BEGINNING and ends at END."
  (interactive "*r")
  (unless guess-language--regexps
    (guess-language-compile-regexps))
  (when (cl-set-exclusive-or guess-language-languages (mapcar #'car guess-language--regexps))
    (guess-language-compile-regexps))
  (let ((tally (cl-loop
                for (lang . regexp) in guess-language--regexps
                collect (cons lang (how-many regexp beginning end)))))
    (car (cl-reduce (lambda (x y) (if (> (cdr x) (cdr y)) x y)) tally))))

(defun guess-language-buffer ()
  "Guess the language of the buffer."
  (guess-language-region (point-min) (point-max)))

(defun guess-language-paragraph ()
  "Guess the language of the current paragraph."
  (let ((beginning (save-excursion (guess-language-backward-paragraph) (point)))
        (end       (save-excursion (guess-language-forward-paragraph) (point))))
    (guess-language-region beginning end)))

(defun guess-language-line ()
  "Guess the language of the current buffer line."
  (let ((beginning (save-excursion (beginning-of-line) (point)))
        (end       (save-excursion (end-of-line) (point))))
    (guess-language-region beginning end)))

(defun guess-language ()
  "Guess language of the current paragraph.

Calls the functions in
`guess-language-after-detection-functions`.  These functions may
switch the dictionary of the spell checker and do other useful
things like changing the keyboard layout or input method."
  (interactive)
  (let ((beginning (save-excursion (guess-language-backward-paragraph) (point)))
        (end       (save-excursion (guess-language-forward-paragraph)  (point))))
    (when (> (- end beginning) guess-language-min-paragraph-length)
      (let ((lang (guess-language-region beginning end)))
        (run-hook-with-args 'guess-language-after-detection-functions lang beginning end)
        (setq guess-language-current-language lang)))))

(defun guess-language--idle-begin (buf win tick beginning)
  "Run guess-langauge unless we left or changed the current paragraph."
  (when (and (eq buf (current-buffer))
             (eq win (selected-window))
             (eq tick (buffer-chars-modified-tick))
             (eq beginning (save-excursion (guess-language-backward-paragraph) (point))))
    (guess-language)
    ;; when running from idle timer, run the post command right away
    (guess-language--post-command-h)))

(defun guess-language-function (_beginning _end _doublon)
  "Wrapper for `guess-language'.

Ignores arguments of `flyspell-incorrect-hook' that we don't need and
adds an idle delay."
  (if (and guess-language-idle-delay (> guess-language-idle-delay 0))
      (run-with-idle-timer guess-language-idle-delay nil
                           #'guess-language--idle-begin
                           (current-buffer) (selected-window)
                           (buffer-chars-modified-tick)
                           (save-excursion (guess-language-backward-paragraph) (point)))
    (guess-language))
  ;; Return nil because flyspell may otherwise not highlight incorrect
  ;; words:
  nil)

(defun guess-language--post-command-h ()
  "The `post-command-hook' used by guess-language.

Used by `guess-language-switch-flyspell-function' to recheck the
spelling of the current paragraph after switching dictionary."
  (funcall guess-language--post-command-h))

(defun guess-language-switch-flyspell-function (lang beginning end)
  "Switch the Flyspell dictionary and recheck the current paragraph.

This is only done if the new language is different from the
previous language.  Otherwise, nothing happens.

LANG is the ISO 639-1 code of the language (as a
symbol).  BEGINNING and END are the endpoints of the region in
which LANG was detected."
  (let* ((old-dictionary (cadr (assq guess-language-current-language guess-language-langcodes)))
         (new-dictionary (cadr (assq lang guess-language-langcodes))))
    (unless (string= old-dictionary new-dictionary)
      (ispell-change-dictionary new-dictionary)
      ;; Flyspell the region with the new dictionary after we return
      ;; from flyspell-incorrect-hook that called us. Otherwise, the
      ;; word at point is highlighted as incorrect even if it is
      ;; correct according to the new dictionary.
      (setq guess-language--post-command-h
            (lambda ()
              (setq guess-language--post-command-h #'ignore)
              (let ((flyspell-issue-welcome-flag nil)
                    (flyspell-issue-message-flag nil)
                    (flyspell-incorrect-hook nil)
                    (flyspell-large-region 1))
                (with-local-quit
                  (flyspell-region beginning end))))))))

(defun guess-language-switch-typo-mode-function (lang _beginning _end)
  "Switch the language used by typo-mode.

LANG is the ISO 639-1 code of the language (as a
symbol).  BEGINNING and END are the endpoints of the region in
which LANG was detected (not used)."
  (when (bound-and-true-p typo-mode)
    (let* ((typo-lang (cl-caddr (assq lang guess-language-langcodes))))
      (typo-change-language typo-lang))))

(defun guess-language-flyspell-buffer-wrapper (orig-fun &rest args)
  "Do not guess language when an unknown word is encountered
during `flyspell-buffer'."
  (let ((flyspell-incorrect-hook nil))
    (apply orig-fun args)))

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
  :lighter (:eval (format " %s"
                   ;; Assume default language is the first in languages list
                   (let* ((language-to-show (or
                                             guess-language-current-language
                                             (car guess-language-languages)))
                          (entry (assq language-to-show guess-language-langcodes)))
                     (or
                      (nth 3 entry)
                      ;; Options for users of old configurations:
                      (nth 2 entry)
                      (nth 1 entry)
                      "No langs"))))
  :global nil
  (if guess-language-mode
      (progn
        (add-hook 'flyspell-incorrect-hook #'guess-language-function nil t)
        ;; Depth of 92 to ensure placement after flyspell's PCH
        (add-hook 'post-command-hook #'guess-language--post-command-h 92 t)
        (advice-add 'flyspell-buffer :around #'guess-language-flyspell-buffer-wrapper))
    (remove-hook 'flyspell-incorrect-hook #'guess-language-function t)
    (remove-hook 'post-command-hook #'guess-language--post-command-h t)
    (advice-remove 'flyspell-buffer #'guess-language-flyspell-buffer-wrapper)))

(defun guess-language-mark-lines (&optional highlight)
  "Guess language on all lines in the buffer and mark them.

If HIGHLIGHT is non-nil, lines that are not in the same language
as the overall buffer are marked red, other lines are marked
green.  Marking is done with overlays which can be removed using
the function `remove-overlays'.

This primary purpose of this command is to aid debugging and
improvement of the language identification algorithm.  Interface
and implementation details may change in the future."
  (interactive)
  (remove-overlays)
  (let ((buffer-lang (guess-language-buffer)))
    (save-excursion
      (goto-char (point-min))
      (while (save-excursion (= 0 (forward-line 1)))
        (unless (= 0 (string-match-p "^[[:blank:]]*$" (thing-at-point 'line)))
          (let* ((beginning (save-excursion (beginning-of-line) (point)))
                 (end       (save-excursion (end-of-line) (point)))
                 (lang      (guess-language-region beginning end))
                 (overlay   (make-overlay beginning end)))
            (overlay-put overlay 'before-string (concat (symbol-name lang) ": "))
            (overlay-put overlay 'face '(:background "grey90"))
            (when highlight
              (if (eq buffer-lang lang)
                  (overlay-put overlay 'face '(:background "green"))
                (overlay-put overlay 'face '(:background "red"))))))
        (forward-line 1)))))

(provide 'guess-language)

;; Local Variables:
;; coding: utf-8
;; indent-tabs-mode: nil
;; End:

;;; guess-language.el ends here
