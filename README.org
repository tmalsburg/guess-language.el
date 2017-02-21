
[[https://melpa.org/#/guess-language][file:https://melpa.org/packages/guess-language-badge.svg]]

* guess-language: Emacs minor mode for robust automatic language detection

Emacs minor mode that detects the language of what you're typing.  Automatically switches the spell checker and typo-mode (if present).

*Key features:*
- Detection algorithm is robust, efficient, and dead simple.  Based on
  character trigrams.
- Support for many languages.  More can be easily added.
- Stays out of your way.  Set up once, then forget it exists.
- Works with documents written in multiple languages.

-----

I write a lot of text in multiple languages and was getting tired of constantly having to switch the dictionary of my spell-checker.  In true Emacs spirit, I decided to dust off my grandpa's parentheses and wrote some code to address this problem.  The result is ~guess-language-mode~, a minor mode for Emacs that guesses the language of the current paragraph and then changes the dictionary of ispell and the language settings of typo-mode (if present).  It also reruns Flyspell on the current paragraph, but only on that paragraph because I want to leave paragraphs in other languages untouched.  Language guessing is triggered when Flyspell detects an unknown word, but only if the paragraph has enough material to allow for robust detection of the language (~ 35 characters).

Currently, the following languages are supported: Arabic, Czech, Danish, Dutch, English, Finnish, French, German, Italian, Norwegian, Polish, Portuguese, Russian, Slovak, Slovenian, Spanish, Swedish.  It is very easy to add more languages and this repository includes the necessary language statistics for 49 additional languages.  (These were copied from [[https://github.com/kent37/guess-language][guess_language.py]].)

** Prerequisites

This mode assumes that Flyspell is activated and configured for all relevant languages, i.e., those listed in ~guess-language-languages~.  If [[https://github.com/jorgenschaefer/typoel][typo-mode]] is present, guess-language also changes the language there.  Typo-mode is not a dependency, though.

** Installation

Guess-language-mode is available through [[https://melpa.org/#/guess-language][MELPA]].

** Configuration

*** Language settings

#+BEGIN_SRC elisp
(require 'guess-language)

;; Optionally:
(setq guess-language-languages '(en de))
(setq guess-language-min-paragraph-length 35)
#+END_SRC

~guess-language-languages~ defines the candidate languages that should be considered.  It is recommended to only include languages that are actually used because this improves performance.  Languages are identified using ISO 639-1 codes (see table below).

~guess-language-min-paragraph-length~ specifies the minimal length that a paragraph needs to have before guess-language-mode starts guessing.  Based on some informal tests texts shorter than 30 characters are not enough to give good results.  However, above 40 characters the algorithm performs well.  Of course, these numbers depend on the target language (some are easier to detect than others) and on the (number of) candidate languages that are considered.  (Open the files in the directory ~testdata~ and do ~M-x guess-language-mark-lines~ to see for yourself.)

For each language, there is a default Ispell dictionary that guess-language-mode tries to use.  However, for some languages there are several dictionaries available and guess-language can’t know which one you’d like to use.  For example, there are several different dictionaries for German and for English.  If the dictionary that guess-language-mode uses by default is not present, you will get an error message like the following:

#+BEGIN_SRC elisp
Error in post-command-hook (flyspell-post-command-hook): (error "Undefined dictionary: en")
#+END_SRC

In this case, use the variable ~guess-language-langcodes~ to tell guess-language-mode which dictionary should be used instead.  For example, use the following definition if you want to use British English and Swiss German:

#+BEGIN_SRC elisp
(setq guess-language-langcodes
  '((en . ("en_GB" "English"))
    (de . ("de_CH" "German"))))
#+END_SRC

The key of each entry in this alist is an ISO 639-1 language code.  The first element of the value is the name of the dictionary that should be used (i.e., what you would enter when setting the language via ~M-x ispell-change-dictionary~).  The second element is the name of the language setting that should be used with typo-mode (if present).  If a language is not supported by typo-mode or if you are not using typo-mode, enter ~nil~.

For a list of all dictionaries available for spell-checking, use the following:

#+BEGIN_SRC org
(mapcar 'car ispell-dictionary-alist)
#+END_SRC

Languages that are currently supported by guess-language-mode:

| Language   | IDO 639-1 code | Default Ispell dictionary | Default typo-mode setting |
|------------+----------------+---------------------------+---------------------------|
| Arabic     | ~ar~           | ar                        |                           |
| Czech      | ~cs~           | czech                     | Czech                     |
| Danish     | ~da~           | dansk                     |                           |
| Dutch      | ~nl~           | nederlands                |                           |
| English    | ~en~           | en                        | English                   |
| Finnish    | ~fi~           | finnish                   | Finnish                   |
| French     | ~fr~           | francais                  | French                    |
| German     | ~de~           | de                        | German                    |
| Italian    | ~it~           | italiano                  | Italian                   |
| Norwegian  | ~nb~           | norsk                     |                           |
| Polish     | ~pl~           | polish                    |                           |
| Portuguese | ~pt~           | portuguese                |                           |
| Russian    | ~ru~           | russian                   | Russian                   |
| Slovak     | ~sk~           | slovak                    |                           |
| Slovenian  | ~sl~           | slovenian                 |                           |
| Spanish    | ~es~           | spanish                   |                           |
| Swedish    | ~sv~           | svenska                   |                           |

*** Custom functions to be run when a new language is detected

While changing the spell-checker’s dictionary is the main purpose of guess-language, there are other things that a user might want to do when a new language is detected, for instance, a user might want to change the input method.  Things like that can be easily achieved by adding custom functions to the hook ~guess-language-after-detection-functions~.  Functions on this hook take three arguments:

| ~LANG~      | the language that was detected                                     |
| ~BEGINNING~ | the beginning of the region in which the new language was detected |
| ~END~       | the end of the region                                              |

Template:

#+BEGIN_SRC elisp
(defun my-custom-function (lang beginning end)
  (do-something))

(add-hook 'guess-language-after-detection-functions #'my-custom-function)
#+END_SRC

** Usage

Activate ~guess-language-mode~ in the buffer in which you want to use it.  To activate it automatically in buffers containing text (as opposed to code), add guess-language mode to ~text-mode-hook~:

#+BEGIN_SRC elisp
(add-hook 'text-mode-hook (lambda () (guess-language-mode 1)))
#+END_SRC

*** Changing the voice used by the Festival text-to-speech system

The code snipped below illustrates how guess-language can be configured to automatically change the voice used by the text-to-speech engine [[http://www.cstr.ed.ac.uk/projects/festival/][Festival]] (install [[https://www.emacswiki.org/emacs/festival.el][festival.el]] for this to work):

#+BEGIN_SRC elisp
(defun guess-language-switch-festival-function (lang beginning end)
  "Switch the voice used by festival.

LANG is the ISO 639-1 code of the language (as a
symbol).  BEGINNING and END are the endpoints of the region in
which LANG was detected but these are ignored."
  (when (and (featurep 'festival)
             (festivalp))
    (pcase lang
      ('en (festival-voice-english-female))
      ('de (festival-voice-german-female)))))

(add-hook 'guess-language-after-detection-functions #'guess-language-switch-festival-function)
#+END_SRC

The ~pcase~ needs to be modified to use the voiced that are installed on your system.  Refer to the documentation of Festival for details.


