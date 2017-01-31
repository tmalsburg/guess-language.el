
I write a lot of text in multiple languages and I was getting tired of switching the dictionary of my spell-checker all the time.  In true Emacs spirit, I decided to do something about this and dusted off my grandpa's parentheses.  The code in this repository guesses the language of the current paragraph and then changes the dictionary of ispell and the language-specific settings of typo-mode.  It also reruns flyspell on the current paragraph, but only on that paragraph because some documents are written in multiple languages and I want to leave paragraphs in other languages untouched.

This is currently a proof of concept and only supports two languages, English and German.  However, it should be very easy to add more languages.
