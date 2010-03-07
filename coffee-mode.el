;; Major thanks to http://xahlee.org/emacs/elisp_syntax_coloring.html
;; the instructions.


;;
;; Define Language Syntax
;;

;; Assignment
(setq coffee-type-regexp ".+?:")

;; Instance variables (implicit this)
(setq coffee-constant-regexp "@\\w*\\|this")

;; Unused
(setq coffee-event-regexp "")

;; Unused
(setq coffee-functions-regexp "")

;; JavaScript Keywords
(setq coffee-js-keywords
      '("if" "else" "true" "false" "new" "return" "try" "catch"
        "finally" "throw" "break" "continue" "for" "in" "while"
        "delete" "instanceof" "typeof" "switch" "super" "extends"
        "class"))

;; Reserved keywords either by JS or CS.
(setq coffee-js-reserved
      '("case" "default" "do" "function" "var" "void" "with"
        "const" "let" "debugger" "enum" "export" "import" "native"
        "__extends" "__hasProp"))

;; CoffeeScript keywords.
(setq coffee-cs-keywords
      '("then" "unless" "yes" "no" "on" "off" "and" "or" "is"
        "isnt" "not" "of" "by" "where" "when"))

;; Regular expression combining the above three lists.
(setq coffee-keywords-regexp (regexp-opt
                                (append
                                 coffee-js-reserved
                                 coffee-js-keywords
                                 coffee-cs-keywords) 'words))


;; Create the list for font-lock.
;; Each class of keyword is given a particular face
(setq coffee-font-lock-keywords
      `(
        (,coffee-type-regexp . font-lock-type-face)
        (,coffee-constant-regexp . font-lock-constant-face)
        (,coffee-event-regexp . font-lock-builtin-face)
        (,coffee-functions-regexp . font-lock-function-name-face)
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
;; Define Major Mode
;;

(define-derived-mode coffee-mode fundamental-mode
  "coffee-mode"
  "Major mode for editing CoffeeScript..."

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; modify the keymap
  (define-key coffee-mode-map [remap comment-dwim] 'coffee-comment-dwim)

  ;; perl style comment: "# ..."
  (modify-syntax-entry ?# "< b" coffee-mode-syntax-table)
  (modify-syntax-entry ?\n "> b" coffee-mode-syntax-table)
  (setq comment-start "#")

  ;; single quote strings
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)
  (modify-syntax-entry ?' "\"" coffee-mode-syntax-table)

  ;; regular expressions
  (modify-syntax-entry ?/ "\"" coffee-mode-syntax-table)
  (modify-syntax-entry ?/ "\"" coffee-mode-syntax-table)

  ;; clear memory
  (setq coffee-keywords-regexp nil)
  (setq coffee-types-regexp nil)
  (setq coffee-constants-regexp nil)
  (setq coffee-events-regexp nil)
  (setq coffee-functions-regexp nil))
