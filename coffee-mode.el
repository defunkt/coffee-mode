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

(defvar coffee-keywords-regexp (regexp-opt
                                (append
                                 coffee-js-reserved
                                 coffee-js-keywords
                                 coffee-cs-keywords) 'words))

(setq coffee-types '(""))
(defvar coffee-type-regexp (regexp-opt coffee-types 'words))

(setq coffee-constant-regexp "")

(setq coffee-events '(""))
(defvar coffee-event-regexp (regexp-opt coffee-events 'words))

(setq coffee-functions '(""))
(defvar coffee-functions-regexp (regexp-opt coffee-functions 'words))

;; create the list for font-lock.
;; each class of keyword is given a particular face
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

;; define the mode
(define-derived-mode coffee-mode fundamental-mode
  "coffee-mode"
  "Major mode for editing CoffeeScript..."

  ;; code for syntax highlighting
  (setq font-lock-defaults '((coffee-font-lock-keywords)))

  ;; clear memory
  (setq coffee-keywords-regexp nil)
  (setq coffee-types-regexp nil)
  (setq coffee-constants-regexp nil)
  (setq coffee-events-regexp nil)
  (setq coffee-functions-regexp nil))
