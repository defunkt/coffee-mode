;;; highlight.el --- Test for highlighting of coffee-mode.el

;; Copyright (C) 2013 by Syohei YOSHIDA

;; Author: Syohei YOSHIDA <syohex@gmail.com>

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

;;; Code:

(eval-when-compile
  (require 'cl))

(require 'ert)
(require 'test-helper)

(require 'coffee-mode)

;;
;; This and class member highlight
;;

(ert-deftest this-and-instance-variable ()
  "Highlight `this' and instance variable(`@foo')"

  (dolist (keyword '("this" "@foo" "@foo1234" "@foo_bar" "@FoO"))
    (with-coffee-temp-buffer
      keyword
      (should (face-at-cursor-p 'font-lock-variable-name-face)))))

(ert-deftest this-and-instance-variable-invalid ()
  "Invalid `this' and instance variable highlighting"

  (dolist (keyword '("thiss" "@-" "@+"))
    (with-coffee-temp-buffer
      keyword
      (should-not (face-at-cursor-p 'font-lock-variable-name-face)))))

;;
;; Prototype Access
;;

(ert-deftest prototype-access ()
  "Prototype access"

  (with-coffee-temp-buffer
    " Foo::bar "

    ;; XXX Current implementation highlight this as assign-regexp
    ;; And What is correct highlighting ?? `Foo' ? `Foo::' ? `Foo::bar' ?
    (forward-cursor-on "Foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "::")
    (should (face-at-cursor-p 'font-lock-variable-name-face))
    (forward-char 1)
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "bar")
    (should-not (face-at-cursor-p 'font-lock-variable-name-face))))

(ert-deftest prototype-access-without-property ()
  "Prototype without property"

  (with-coffee-temp-buffer
    "Foo:: "

    (forward-cursor-on "Foo")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "::")
    (should (face-at-cursor-p 'font-lock-variable-name-face))))

;;
;; Assignment
;;

(ert-deftest assignment-with-no-spaces ()
  "assignment with no spaces"

  (with-coffee-temp-buffer
    "
foo =
  key: value
"

    (forward-cursor-on "key")
    (should (face-at-cursor-p 'font-lock-type-face))

    ;; XXX Should `colon' be highlighted ? (now highlighted)
    (forward-cursor-on ":")
    (should (face-at-cursor-p 'font-lock-type-face))

    (forward-cursor-on "value")
    (should-not (face-at-cursor-p 'font-lock-type-face))))

(ert-deftest assignment-with-spaces ()
  "assignment with spaces"

  (with-coffee-temp-buffer
    "
foo =
  key  : value
"

    (forward-cursor-on "key")
    (should (face-at-cursor-p 'font-lock-type-face))

    ;; XXX Same as above
    (forward-cursor-on ":")
    (should (face-at-cursor-p 'font-lock-type-face))

    (forward-cursor-on "value")
    (should-not (face-at-cursor-p 'font-lock-type-face))))

;;
;; Local assignment
;;

(ert-deftest local-assignment-with-no-spaces ()
  "local assignment with no spaces"

  (with-coffee-temp-buffer
    "foo=10"

    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "=")
    (should-not (face-at-cursor-p 'font-lock-type-face))

    (forward-cursor-on "10")
    (should-not (face-at-cursor-p 'font-lock-type-face))))

(ert-deftest local-assignment-with-spaces ()
  "local assignment with spaces"

  (with-coffee-temp-buffer
    "foo = 10"

    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "=")
    (should-not (face-at-cursor-p 'font-lock-type-face))

    (forward-cursor-on "10")
    (should-not (face-at-cursor-p 'font-lock-type-face))))

(ert-deftest local-assignment-with-arrow ()
  "Don't highlight if `>' is following '='"

  (with-coffee-temp-buffer
    "foo => 10"

    (should-not (face-at-cursor-p 'font-lock-variable-name-face))))

;;
;; Lambda expression
;;

(ert-deftest lambda-expression-fat-arrow ()
  "Highlight lambda expression with fat arrow"

  (with-coffee-temp-buffer
    "foo =>
      \"bar\""

    (forward-cursor-on "=>")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

(ert-deftest lambda-expression-thin-arrow ()
  "Highlight lambda expression with thin arrow"

  (with-coffee-temp-buffer
    "foo ->
      \"bar\""

    (forward-cursor-on "->")
    (should (face-at-cursor-p 'font-lock-function-name-face))))

;;
;; String
;;

(ert-deftest string-double-quoted ()
  "Highlight string double quoted"

  (with-coffee-temp-buffer
    " \"foo\" "

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-string-face))))

(ert-deftest string-double-quoted-with-escape ()
  "Highlight string double quoted with escape"

  (with-coffee-temp-buffer
    " \"  \\\"foo\\\"  \" "

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-string-face))))

(ert-deftest string-single-quoted ()
  "Highlight string single quoted"

  (with-coffee-temp-buffer
    " 'foo' "

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-string-face))))

(ert-deftest string-single-quoted-with-escape ()
  "Highlight string single quoted with escape"

  (with-coffee-temp-buffer
    " ' \\'foo\\' '  \" "

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-string-face))))

;;
;; String Interpolation(#120)
;;

(ert-deftest string-interpolation ()
  "Highlight lambda expression with thin arrow"

  (with-coffee-temp-buffer
    " \"foo #{var} bar\" "

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "var")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "bar")
    (should (face-at-cursor-p 'font-lock-string-face))))

;;
;; Keywords
;;

(ert-deftest keywords-js-keywords ()
  "Highlight JavaScript keywords"

  (dolist (js-keyword '("if" "else" "new" "return" "try" "catch"
                        "finally" "throw" "break" "continue" "for" "in" "while"
                        "delete" "instanceof" "typeof" "switch" "super" "extends"
                        "class" "until" "loop"))
    (with-coffee-temp-buffer
      js-keyword
      (should (face-at-cursor-p 'font-lock-keyword-face)))))

(ert-deftest keywords-js-reserved ()
  "Highlight JavaScript reserved words"

  (dolist (js-reserved '("case" "default" "do" "function" "var" "void" "with"
                         "const" "let" "debugger" "enum" "export" "import" "native"
                         "__extends" "__hasProp"))
    (with-coffee-temp-buffer
      js-reserved
      (should (face-at-cursor-p 'font-lock-keyword-face)))))

(ert-deftest keywords-coffee-reserved ()
  "Highlight CoffeeScript reserved words"

  (dolist (cs-reserved '("then" "unless" "and" "or" "is" "own"
                         "isnt" "not" "of" "by" "when"))
    (with-coffee-temp-buffer
      cs-reserved
      (should (face-at-cursor-p 'font-lock-keyword-face)))))

(ert-deftest keywords-iced-coffee-keywords ()
  "Highlight Iced CoffeeScript keywords"

  (dolist (iced-keyword '("await" "defer"))
    (with-coffee-temp-buffer
      iced-keyword
      (should (face-at-cursor-p 'font-lock-keyword-face)))))

;;
;; Boolean highlight
;;

(ert-deftest boolean ()
  "boolean highlight"

  (dolist (keyword '("true" "false" "yes" "no" "on" "off" "null" "undefined"))
    (with-coffee-temp-buffer
      keyword
      (should (face-at-cursor-p 'font-lock-constant-face)))))

;;
;; Single line comment
;;

(ert-deftest single-line-comment ()
  "Highlight single line comment"

  (with-coffee-temp-buffer
    "
# single_line_comment
out_of_single_line_comment
"
    (forward-cursor-on "single_line_comment")
    (should (face-at-cursor-p 'font-lock-comment-face))

    (forward-cursor-on "out_of_single_line_comment")
    (should-not (face-at-cursor-p 'font-lock-comment-face))))

;;
;; Block Comment Tests(#146, #149)
;;

(ert-deftest block-comment-in-comment ()
  "In block comment"

  (with-coffee-temp-buffer
    "
###
block_comment
###
"
    (forward-cursor-on "block_comment")
    (should (face-at-cursor-p 'font-lock-comment-face))))

(ert-deftest block-comment-before-comment ()
  "Before block comment"
  (with-coffee-temp-buffer
    "
before_comment
###
block_comment
###
"
    (forward-cursor-on "before_comment")
    (should-not (face-at-cursor-p 'font-lock-comment-face))))

(ert-deftest block-comment-after-comment ()
  "After block comment"
  (with-coffee-temp-buffer
    "
###
block_comment
###
after_comment
"
    (forward-cursor-on "after_comment")
    (should-not (face-at-cursor-p 'font-lock-comment-face))))

;;
;; Regular Expression Tests (#141)
;;

(ert-deftest regular-expression-in-regexp ()
  "Face in regular expression literal"
  (with-coffee-temp-buffer
    "/foo/"
    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-constant-face))))

(ert-deftest regular-expression-after-regexp ()
  "After regular expression literal"
  (with-coffee-temp-buffer
    "/foo/ "
    (goto-char (point-max))
    (should-not (face-at-cursor-p 'font-lock-constant-face))))

(ert-deftest regular-expression-with-double-quote ()
  "Regular expression with double quote"
  (with-coffee-temp-buffer
    "/\"foo/ "
    (forward-cursor-on "\"")
    (should (face-at-cursor-p 'font-lock-constant-face))

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-constant-face))

    (goto-char (point-max))
    (should-not (face-at-cursor-p 'font-lock-constant-face))))

(ert-deftest regular-expression-with-hash-mark ()
  "Regular expression with hash mark"
  (with-coffee-temp-buffer
    "/# foo / "
    (forward-cursor-on "#")
    (should (face-at-cursor-p 'font-lock-constant-face))

    (forward-cursor-on "foo")
    (should (face-at-cursor-p 'font-lock-constant-face))))

;;
;; Block Strings(#159)
;;

(ert-deftest block-strings-single-line ()
  "Block Strings in single line"
  (with-coffee-temp-buffer
    " \"\"\"<a class=\"btn btn-mini\" href=\"#{data}\"<i class=\"icon-search\"></i></a>\"\"\" block-strings-end"
    (forward-cursor-on "<a")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "btn")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "data")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "<i")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "icon-search")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "block-strings-end")
    (should-not (face-at-cursor-p 'font-lock-string-face))))

(ert-deftest block-strings-multiple-line ()
  "Block Strings in multiple lines"
  (with-coffee-temp-buffer
    "
html = \"\"\"
       <strong>
         cup of #{data}
       </strong>
       \"\"\"
block-strings-end
"
    (forward-cursor-on "html")
    (should-not (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "<strong>")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "data")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "</strong>")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "block-strings-end")
    (should-not (face-at-cursor-p 'font-lock-string-face))
    (should (face-at-cursor-p nil))))

(ert-deftest block-strings-multiple-line-with-double-quote ()
  "Block Strings in multiple lines with double quote"
  (with-coffee-temp-buffer
    " \"\"\"
      <a class=\"btn btn-mini\" href=\"#{data}\"<i class=\"icon-search\"></i></a>
      \"\"\"
 block-strings-end"
    (forward-cursor-on "<a")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "btn")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "data")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "<i")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "icon-search")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "block-strings-end")
    (should-not (face-at-cursor-p 'font-lock-string-face))))

(ert-deftest block-strings-double-blocks ()
  "Highligiting double Block Strings"
  (with-coffee-temp-buffer
    "
a  = \"\"\"
     alice
     \"\"\"

b = bob

c  = \"\"\"
     carol
     \"\"\"
block-strings-end
"
    (forward-cursor-on "alice")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "b =")
    (should (face-at-cursor-p 'font-lock-variable-name-face))

    (forward-cursor-on "carol")
    (should (face-at-cursor-p 'font-lock-string-face))

    (forward-cursor-on "block-strings-end")
    (should-not (face-at-cursor-p 'font-lock-string-face))
    (should (face-at-cursor-p nil))))

;;; highlight.el end here
