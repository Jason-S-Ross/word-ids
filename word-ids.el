;;; word-ids.el --- Generates somewhat unique strings using words -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2022 Jason Ross
;;
;; Author: Jason Ross <https://github.com/Jason-S-Ross>
;; Maintainer: Jason Ross <jasonross1024@gmail.com>
;; Created: January 15, 2022
;; Modified: January 15, 2022
;; Version: 0.0.1
;; Keywords: abbrev bib c calendar comm convenience data docs emulations extensions faces files frames games hardware help hypermedia i18n internal languages lisp local maint mail matching mouse multimedia news outlines processes terminals tex tools unix vc wp
;; Homepage: https://github.com/Jason-S-Ross/word-ids
;; Package-Requires: ((emacs "24.4"))
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;;
;; This is not cryptographic. I am not a security expert. Don't use this for anything
;; important.
;;
;;  Description
;;  Generates a random string with approximately nbits of entropy using the user's
;;  Ispell dictionary.
;;
;;; Code:

(defgroup word-ids nil
  "Word-ids customization."
  :group 'word-ids
  :package-version '(word-ids . "0.1.0"))

(defcustom word-ids-dictionary-path
 nil
  "Path to the dictionary on the computer."
  :group 'word-ids
  :type 'string
  :package-version '(word-ids . "0.1.0"))

(with-eval-after-load 'ispell
  (setq word-ids-dictionary-path
        (or ispell-complete-word-dict
            ispell-alternate-dictionary)))


(defvar word-ids-num-words nil
  "Number of words in the dictionary. Cached value.")

(defun word-ids-random-words (nbits)
  "Generate random words using the system dictionary.
NBITS is used to approximate the entropy of the resulting string."
  (let* ((lookup-dict word-ids-dictionary-path)
         (total-words
          (or word-ids-num-words
              (setq word-ids-num-words
                    (string-to-number
                     (shell-command-to-string
                      (format  "wc -l %s | cut -d' ' -f1" lookup-dict))))))
         (required-words
          (ceiling (* nbits (log 2 total-words))))
         numbers)
    (dotimes (_i required-words)
      (push (+ (mod (random) total-words) 1) numbers))
    numbers
    (let* ((command
            (format
             "sed -n '%s' %s"
             (mapconcat (lambda (n) (format "%sp;" n)) numbers " ")
             lookup-dict))
           (results
            (with-temp-buffer
              (insert (shell-command-to-string command))
              (set-mark (point-min))
              (replace-string "'" "" nil (point-min) (point-max))
              (set-mark (point-min))
              (upcase-initials-region (point-min) (point-max) )
              (buffer-string)))
           (result-list (split-string results "\n")))
      (mapconcat #'identity result-list ""))))

(defun word-ids-get-unique-words (nbits)
  "Prompts for user to OK the words. NBITS is the approximate bits of entropy.

This function prompts in a loop in case there are any offensive words."
  (let (result)
    (while
        (not
         (string=
          (read-string
           (format
            "Generated \"%s\". OK? (y/n) "
            (setq result (word-ids-random-words nbits))))
          "y")))
    result))


(provide 'word-ids)
;;; word-ids.el ends here
