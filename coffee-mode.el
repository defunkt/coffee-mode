;;; coffee-mode.el --- Major mode to edit CoffeeScript files in Emacs

;; Copyright (C) 2010 Chris Wanstrath

;; Version 0.1.0
;; Keywords: CoffeeScript major mode
;; Author: Chris Wanstrath <chris@ozmm.org>
;; URL: http://github.com/defunkt/coffee-script

;; This file is not part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary

;; For commentary please see the README.md or
;; http://github.com/defunkt/coffee-mode#readme

;;; Installation

;; In your shell:

;;     $ cd ~/.emacs.d/vendor
;;     $ git clone git://github.com/defunkt/coffee-mode.git

;; In your emacs config:

;;     (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
;;     (require 'coffee-mode)

;;; Thanks

;; Major thanks to http://xahlee.org/emacs/elisp_syntax_coloring.html
;; the instructions.

;; Also thanks to Jason Blevins's markdown-mode.el for guidance.

(require 'easymenu)
(require 'font-lock)

;;
;; Customizable Variables
;;

(defconst coffee-mode-version "0.1.0"
  "The version of this `coffee-mode'.")

(defvar coffee-mode-hook nil
  "A hook for you to run your own code when the mode is loaded.")

(defvar coffee-command "coffee"
  "The CoffeeScript command used for evaluating code. Must be in your
path.")

(defvar coffee-repl-args '("-i")
  "The command line arguments to pass to `coffee-command' to start a REPL.")

(defvar coffee-command-args '("-s" "-p" "--no-wrap")
  "The command line arguments to pass to `coffee-command' to get it to
print the compiled JavaScript.")

(defun coffee-command-full ()
  "The full `coffee-command' complete with args."
  (mapconcat 'identity (append (list coffee-command) coffee-command-args) " "))

(defvar coffee-js-mode 'js2-mode
  "The mode to use when viewing compiled JavaScript.")

(defvar coffee-compiled-buffer-name "*coffee-compiled*"
  "The name of the scratch buffer used when compiling CoffeeScript.")

(defvar coffee-mode-map (make-keymap)
  "Keymap for CoffeeScript major mode.")

;;
;; Commands
;;

(defun coffee-repl ()
  "Launch a CoffeeScript REPL using `coffee-command' as an inferior mode."
  (interactive)

  (unless (comint-check-proc "*CoffeeScript*")
    (set-buffer
     (apply 'make-comint "CoffeeScript"
            coffee-command nil coffee-repl-args)))

  (pop-to-buffer "*CoffeeScript*"))

(defun coffee-compile-buffer ()
  "Compiles the current buffer and displays the JS in the other buffer."
  (interactive)
  (save-excursion
    (coffee-compile-region (point-min) (point-max))))

(defun coffee-compile-region (start end)
  "Compiles a region and displays the JS in the other buffer."
  (interactive "r")

  (let ((buffer (get-buffer coffee-compiled-buffer-name)))
    (when buffer
      (kill-buffer buffer)))

  (call-process-region start end coffee-command nil
                       (get-buffer-create coffee-compiled-buffer-name)
                       nil
                       "-s" "-p" "--no-wrap")
  (switch-to-buffer-other-frame (get-buffer coffee-compiled-buffer-name))
  (funcall coffee-js-mode)
  (beginning-of-buffer))

(defun coffee-show-version ()
  "Prints the `coffee-mode' version."
  (interactive)
  (message (concat "coffee-mode v" coffee-mode-version)))

(defun coffee-open-reference ()
  "Open browser to CoffeeScript reference."
  (interactive)
  (browse-url "http://jashkenas.github.com/coffee-script/"))

(defun coffee-open-github ()
  "Open browser to `coffee-mode' project on GithHub."
  (interactive)
  (browse-url "http://github.com/defunkt/coffee-mode"))

;;
;; Menubar
;;

(easy-menu-define coffee-mode-menu coffee-mode-map
  "Menu for CoffeeScript mode"
  '("CoffeeScript"
    ["Compile Buffer" coffee-compile-buffer]
    ["Compile Region" coffee-compile-region]
    "---"
    ["CoffeeScript reference" coffee-open-reference]
    ["coffee-mode on GitHub" coffee-open-github]
    ["Version" coffee-show-version]
    ))

;;
;; Define Language Syntax
;;

;; Instance variables (implicit this)
(setq coffee-this-regexp "@\\w*\\|this")

;; Assignment
(setq coffee-assign-regexp "\\(\\w\\|\\.\\|_\\| \\|$\\)+?:")

;; Booleans
(setq coffee-boolean-regexp "\\b\\(true\\|false\\|yes\\|no\\|on\\|off\\)\\b")

;; Unused
(setq coffee-regexp-regexp "\\/.+?\\/")

;; JavaScript Keywords
(defvar coffee-js-keywords
      '("if" "else" "new" "return" "try" "catch"
        "finally" "throw" "break" "continue" "for" "in" "while"
        "delete" "instanceof" "typeof" "switch" "super" "extends"
        "class"))

;; Reserved keywords either by JS or CS.
(defvar coffee-js-reserved
      '("case" "default" "do" "function" "var" "void" "with"
        "const" "let" "debugger" "enum" "export" "import" "native"
        "__extends" "__hasProp"))

;; CoffeeScript keywords.
(defvar coffee-cs-keywords
      '("then" "unless" "and" "or" "is"
        "isnt" "not" "of" "by" "where" "when"))

;; Regular expression combining the above three lists.
(defvar coffee-keywords-regexp (regexp-opt
                                (append
                                 coffee-js-reserved
                                 coffee-js-keywords
                                 coffee-cs-keywords) 'words))


;; Create the list for font-lock.
;; Each class of keyword is given a particular face
(setq coffee-font-lock-keywords
      `(
        (,coffee-this-regexp . font-lock-variable-name-face)
        (,coffee-assign-regexp . font-lock-type-face)
        (,coffee-regexp-regexp . font-lock-constant-face)
        (,coffee-boolean-regexp . font-lock-constant-face)
        (,coffee-keywords-regexp . font-lock-keyword-face)

        ;; note: order above matters. `coffee-keywords-regexp' goes last because
        ;; otherwise the keyword "state" in the function "state_entry"
        ;; would be highlighted.
        ))

;;
;; Helper Functions
;;

;; The command to comment/uncomment text
(defun coffee-comment-dwim (arg)
  "Comment or uncomment current line or region in a smart way.
For detail, see `comment-dwim'."
  (interactive "*P")
  (require 'newcomment)
  (let ((deactivate-mark nil) (comment-start "#") (comment-end ""))
    (comment-dwim arg)))

;;
;; Indentation
;;

;; The theory is explained in the README.

(defun coffee-indent-line ()
  "Indent current line as CoffeeScript."
  (interactive)

  (save-excursion
    (let ((prev-indent 0) (cur-indent 0))
      ;; Figure out the indentation of the previous line
      (forward-line -1)
      (setq prev-indent (current-indentation))

      ;; Figure out the current line's indentation
      (forward-line 1)
      (setq cur-indent (current-indentation))

      ;; Shift one column to the left
      (backward-to-indentation 0)
      (insert-tab)

      ;; We're too far, remove all indentation.
      (when (> (- (current-indentation) prev-indent) tab-width)
        (backward-to-indentation 0)
        (delete-region (point-at-bol) (point))))))

(defun coffee-newline-and-indent ()
  "Inserts a newline and indents it to the same level as the previous line."
  (interactive)

  ;; Remember the current line indentation level,
  ;; insert a newline, and indent the newline to the same
  ;; level as the previous line.
  (let ((prev-indent (current-indentation)))
    (newline)
    (insert-tab (/ prev-indent tab-width)))

  (when (coffee-previous-line-is-comment)
    (insert "# ")))

(defun coffee-previous-line-is-comment ()
  "Returns `t' if the previous line is a CoffeeScript comment."
  (save-excursion
    (forward-line -1)
    (coffee-line-is-comment)))

(defun coffee-line-is-comment ()
  "Returns `t' if the current line is a CoffeeScript comment."
  (save-excursion
    (backward-to-indentation 0)
    (= (char-after) (string-to-char "#"))))

;;
;; Define Major Mode
;;

(define-derived-mode coffee-mode fundamental-mode
  "coffee-mode"
  "Major mode for editing CoffeeScript..."

  (define-key coffee-mode-map (kbd "A-r") 'coffee-compile-buffer)
  (define-key coffee-mode-map (kbd "A-R") 'coffee-execute-line)
  (define-key coffee-mode-map (kbd "A-M-r") 'coffee-repl)
  (define-key coffee-mode-map [remap comment-dwim] 'coffee-comment-dwim)
  (define-key coffee-mode-map "\C-m" 'coffee-newline-and-indent)

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; perl style comment: "# ..."
  (modify-syntax-entry ?# "< b" coffee-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" coffee-mode-syntax-table)
  (setq comment-start "#")

  ;; single quote strings
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)

  ;; indentation
  (make-local-variable 'indent-line-function)
  (setq indent-line-function 'coffee-indent-line)

  ;; no tabs
  (setq indent-tabs-mode nil)

  ;; clear memory
  (setq coffee-keywords-regexp nil)
  (setq coffee-types-regexp nil)
  (setq coffee-constants-regexp nil)
  (setq coffee-events-regexp nil)
  (setq coffee-functions-regexp nil))

(provide 'coffee-mode)

;;
;; On Load
;;

;; Run coffee-mode for files ending in .coffee.
(add-to-list 'auto-mode-alist '("\\.coffee$" . coffee-mode))
(add-to-list 'auto-mode-alist '("Cakefile" . coffee-mode))
