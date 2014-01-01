;;; coffee-command.el --- Test for commands of coffee-mode.el

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
(require 'coffee-mode)

;;
;; version
;;

(ert-deftest coffee-mode-version ()
  "Check package version equals to version variable"
  (let* ((library (locate-library "coffee-mode"))
         (version (with-current-buffer (find-file-noselect library)
                    (goto-char (point-min))
                    (when (re-search-forward "^;; Version: \\([[:digit:].]+\\)$" nil t)
                      (match-string-no-properties 1)))))
    (should (string= version coffee-mode-version))))

;;
;; tab command(indentation)
;;

(ert-deftest indentation-with-tab ()
  "
     line1()
       line2()
       line3()
          ^

# Pressing `TAB` will produce the following code:

     line1()
       line2()
         line3()
            ^

# Pressing `TAB` again will produce this code:

     line1()
       line2()
     line3()
        ^
"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
line1()
  line2()
  line3()
"
      (forward-cursor-on "line3")
      (call-interactively 'indent-for-tab-command)

      (should (= (current-column) 4))

      (call-interactively 'indent-for-tab-command)
      (should (= (current-column) 0)))))

;;
;; newline and indent
;;

(ert-deftest newline-and-indent ()
  "
class Animal
            ^

# Pressing enter would produce the following:

class Animal
  ^
"
  (let ((coffee-tab-width 4))
    (with-coffee-temp-buffer
      "
class Animal"

      (goto-char (point-max))
      (call-interactively 'coffee-newline-and-indent)

      (should (= (current-column) 4)))))

(ert-deftest newline-and-indent-indenters-bol ()
  "indenters bol keywords"
  (let ((coffee-tab-width 4))
    (dolist (keyword '("class" "for" "if" "else" "while" "until"
                       "try" "catch" "finally" "switch"))
      (with-coffee-temp-buffer
        (format "\n    %s" keyword)
        (goto-char (point-max))
        (let ((cur-indent (current-indentation)))
          (call-interactively 'coffee-newline-and-indent)
          (should (= (current-column) (+ cur-indent coffee-tab-width))))))))

(ert-deftest newline-and-indent-not-indenters-bol ()
  "indenters bol keywords"
  (let ((coffee-tab-width 4))
    (dolist (keyword '("new" "return"))
      (with-coffee-temp-buffer
        (format "\n    %s" keyword)
        (goto-char (point-max))
        (let ((cur-indent (current-indentation)))
          (call-interactively 'coffee-newline-and-indent)
          (should (= (current-column) cur-indent)))))))

(ert-deftest newline-and-indent-deeper ()
  "
$('#demo').click ->
                   ^

# On enter would produce this:

$('#demo').click ->

  ^
"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
$('#demo').click ->"
      (goto-char (point-max))
      (call-interactively 'coffee-newline-and-indent)

      (should (= (current-column) 2)))))

;;
;; newline and insert comment(#)
;;

(ert-deftest insert-hashmark-after-single-line-comment ()
  "Insert hashmark next line of single line comment"
  (with-coffee-temp-buffer
    "
# foo
"
    (forward-cursor-on "foo")
    (goto-char (line-end-position))
    (coffee-newline-and-indent)

    (back-to-indentation)
    (should (looking-at "^#"))))

(ert-deftest not-insert-hashmark-case ()
  "Don't insert hashmark if previous line is statement comment"
  (with-coffee-temp-buffer
    "
foo = 10 # bar
"
    (forward-cursor-on "bar")
    (goto-char (line-end-position))
    (coffee-newline-and-indent)

    (back-to-indentation)
    (should-not (looking-at "^#"))))

(ert-deftest insert-hashmark-after-single-line-comment-not-three-hashmarks ()
  "Insert hashmark next line of single line comment with not three hashmarks"
  (with-coffee-temp-buffer
    "
## foo

####
"
    (forward-cursor-on "foo")
    (goto-char (line-end-position))
    (coffee-newline-and-indent)

    (back-to-indentation)
    (should (looking-at "^#"))

    (forward-cursor-on "####")
    (goto-char (line-end-position))
    (coffee-newline-and-indent)

    (back-to-indentation)
    (should (looking-at "^#"))))

(ert-deftest not-insert-hashmark-after-block-comment ()
  "Don't insert hash mark next line of block comment line"
  (with-coffee-temp-buffer
    "
###
"
    (forward-cursor-on "###")
    (goto-char (line-end-position))
    (coffee-newline-and-indent)

    (back-to-indentation)
    (should-not (looking-at "^#"))))

(ert-deftest indent-inserted-comment-newline ()
  "indent next line comment"
  (with-coffee-temp-buffer
    "
    # foo
"
    (forward-cursor-on "foo")
    (let ((prev-indent (current-indentation)))
      (coffee-newline-and-indent)
      (back-to-indentation)
      (should (looking-at "#"))
      (should-not (zerop (current-indentation)))
      (should (= prev-indent (current-indentation))))))

(ert-deftest indent-inserted-comment-newline-deep-indent ()
  "indent next line comment deep indent case"
  (with-coffee-temp-buffer
    "
              # foo
"
    (forward-cursor-on "foo")
    (let ((prev-indent (current-indentation)))
      (coffee-newline-and-indent)
      (back-to-indentation)
      (should (looking-at "#"))
      (should-not (zerop (current-indentation)))
      (should (= prev-indent (current-indentation))))))

;;
;; indent left
;;

(ert-deftest left-indent-single-line ()
  "
  foo

# Call `call-indent-shift-left'

foo
"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "  foo"

      (call-interactively 'coffee-indent-shift-left)
      (should (= (current-column) 0)))))

(ert-deftest left-indent-single-line-with-count-parameter ()
  "
    foo

# Call C-u 3 M-x `call-indent-shift-left'

 foo
"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "    foo"

      (coffee-indent-shift-left (line-beginning-position) (line-end-position) 3)

      (back-to-indentation)
      (should (= (current-column) 1)))))

(ert-deftest left-indent-with-region ()
  "
  foo
  bar

# Set region from beginning of `foo' and end of `bar' then call `call-indent-shift-left'

foo
bar
"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
  foo
  bar
"
      (goto-char (point-min))
      (let ((region-start (point))
            (region-end (point-max)))
        (coffee-indent-shift-left region-start region-end)

        (goto-char (point-min))
        (forward-cursor-on "foo")
        (should (= (current-column) 0))

        (forward-cursor-on "bar")
        (should (= (current-column) 0))))))

(ert-deftest left-indent-for-region-with-count-parameter ()
  "
    foo
     bar

# Set region from beginning of `foo' and end of `bar' then
# call C-u 1 M-x `call-indent-shift-left'

   foo
    bar
"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
    foo
     bar
"
      (goto-char (point-min))
      (let ((region-start (point))
            (region-end (point-max)))
        (coffee-indent-shift-left region-start region-end 1)

        (goto-char (point-min))
        (forward-cursor-on "foo")
        (should (= (current-column) 3))

        (forward-cursor-on "bar")
        (should (= (current-column) 4))))))

;;
;; indent right
;;

(ert-deftest right-indent-single-line ()
  "
foo

# Call `call-indent-shift-right'

  foo
"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "foo"

      (call-interactively 'coffee-indent-shift-right)

      (back-to-indentation)
      (should (= (current-column) 2)))))

(ert-deftest right-indent-single-line-with-count-parameter ()
  "
    foo

# Call C-u 3 M-x `call-indent-shift-right'

 foo
"
  (with-coffee-temp-buffer
    "foo"

    (coffee-indent-shift-right (line-beginning-position) (line-end-position) 3)

    (back-to-indentation)
    (should (= (current-column) 3))))

(ert-deftest right-indent-with-region ()
  "
foo
bar

# Set region from beginning of `foo' and end of `bar' then call `call-indent-shift-right'

  foo
  bar
"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
foo
bar
"
      (goto-char (point-min))
      (let ((region-start (point))
            (region-end (point-max)))
        (coffee-indent-shift-right region-start region-end)

        (goto-char (point-min))
        (forward-cursor-on "foo")
        (should (= (current-column) 2))

        (forward-cursor-on "bar")
        (should (= (current-column) 2))))))

(ert-deftest right-indent-for-region-with-count-parameter ()
  "
   foo
  bar

# Set region from beginning of `foo' and end of `bar' then
# call C-u 3 M-x `call-indent-shift-right'

      foo
     bar
"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
   foo
  bar
"
      (goto-char (point-min))
      (let ((region-start (point))
            (region-end (point-max)))
        (coffee-indent-shift-right region-start region-end 3)

        (goto-char (point-min))
        (forward-cursor-on "foo")
        (should (= (current-column) 6))

        (forward-cursor-on "bar")
        (should (= (current-column) 5))))))

;;
;; fill paragraph
;;

(ert-deftest fill-paragraph-with-block-comment ()
  "Block comment should be preserved if `fill-paragraph' is applied to
block comment paragraph"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
func = ->
  ###
  Lorem ipsum dolor sit amet, consectetur adipiscing elit.
  Donec ut tellus et felis vulputate tincidunt.
  ###
"
      (forward-cursor-on "Lorem")
      (fill-paragraph)

      (goto-char (point-min))
      (forward-cursor-on "###")
      (let ((comment-start-line (buffer-substring-no-properties
                                 (point) (line-end-position))))
        (should (string-match-p "###$" comment-start-line))))))

;;
;; forward/backward defun
;;

(ert-deftest beginning-of-defun ()
  "Move to beginning of defun"

  (with-coffee-temp-buffer
   "
foo: (apple, orange) ->
  apple + orange
"
   (forward-cursor-on "orange")
   (coffee-beginning-of-defun)

   (should (looking-at "^foo:"))))

(ert-deftest beginning-of-defun-same-line-not-bol ()
  "Move to beginning of defun in same line but cursor is not beginning of line"

  (with-coffee-temp-buffer
   "
foo: (apple, orange) ->
  apple + orange
"
   (forward-cursor-on "->")
   (coffee-beginning-of-defun)

   (should (looking-at "^foo:"))))

(ert-deftest end-of-defun ()
  "Move to end of defun"

  (with-coffee-temp-buffer
   "
foo: (apple, orange) ->
  apple + orange
"
   (forward-cursor-on "foo")
   (coffee-end-of-block)
   (should (eobp))))

(ert-deftest end-of-defun-multiple-defuns ()
  "Move to end of defun"

  (with-coffee-temp-buffer
   "
foo: (apple) ->
  apple + 10

bar: (melon) ->
  melon + 20
"
   (forward-cursor-on "foo")
   (coffee-end-of-block)

   (save-excursion
     (forward-line 1)
     (should (looking-at "^bar:")))

   (coffee-end-of-block)
   (should (eobp))))

(ert-deftest moving-by-defun-for-nested-defun ()
  "Move by defun for nested defun"

  (with-coffee-temp-buffer
   "
class Foo
  constructor: (name, age) ->
    @name = name
    @age  = age

  hear: (regex, callback) ->
    register regex, callback, 99
"
   (forward-cursor-on "99")

   (coffee-beginning-of-defun)
   (should (looking-at "hear:"))

   (coffee-beginning-of-defun)
   (should (looking-at "constructor:"))

   (save-excursion
     (coffee-end-of-block)
     (forward-line 1)
     (should (looking-at "^\\s-+hear:")))

   (coffee-beginning-of-defun)
   (should (looking-at "^class"))

   (coffee-beginning-of-defun)
   (should (bobp))

   (coffee-end-of-block)
   (should (eobp))))

;;
;; mark defun
;;

(ert-deftest mark-defun ()
  "Mark-defun"

  (with-coffee-temp-buffer
   "
human: (name, age) ->
  @name = name
  @age  = age
"
   (forward-cursor-on "human")
   (let ((start (point)))
     (let ((this-command 'coffee-mark-defun))
       (coffee-mark-defun))
     (should (= (region-beginning) start))
     (should (= (region-end) (point-max))))))

(ert-deftest mark-defun-with-no-argument ()
  "Mark-defun for no argument function"

  (with-coffee-temp-buffer
   "
human: () ->
  @name = 'Alice'
  @age  = 49
"
   (forward-cursor-on "human")
   (let ((start (point)))
     (let ((this-command 'coffee-mark-defun))
       (coffee-mark-defun))
     (should (= (region-beginning) start))
     (should (= (region-end) (point-max))))))

(ert-deftest mark-defun-with-no-spaces ()
  "Mark-defun for no space function"

  (with-coffee-temp-buffer
   "
human:(name,age)->
  @name = name
  @age  = age
"
   (forward-cursor-on "human")
   (let ((start (point)))
     (let ((this-command 'coffee-mark-defun))
       (coffee-mark-defun))
     (should (= (region-beginning) start))
     (should (= (region-end) (point-max))))))

(ert-deftest mark-defun-for-nested-defun ()
  "Move by defun for nested defun"

  (with-coffee-temp-buffer
   "
class Foo
  constructor: (name, age) ->
    @name = name
    @age  = age

  hear: (regex, callback) ->
    register regex, callback, 99
"
   (forward-cursor-on "class")
   (let ((start (point)))
     (let ((this-command 'coffee-mark-defun))
       (call-interactively 'coffee-mark-defun))
     (should (= (region-beginning) start))
     (should (= (region-end) (point-max))))


   (forward-cursor-on "hear:")
   (let ((expected-start (point))
         (expected-end (save-excursion
                         (forward-line +1)
                         (goto-char (line-end-position))
                         (1+ (point)))))
     (goto-char (line-end-position))
     (let ((this-command 'coffee-mark-defun))
       (call-interactively 'coffee-mark-defun))
     (should (= (region-beginning) expected-start))
     (should (= (region-end) expected-end)))))

(ert-deftest move-defun-commands-with-prototype-access ()
  "Move defun commands with prototype access"

  (with-coffee-temp-buffer
   "
Foo::bar::baz = (apple, orange) ->
  apple + orange

add = (a, b) ->
  a + b
"
   (forward-cursor-on "Foo")
   (coffee-end-of-block)

   (save-excursion
     (forward-line 1)
     (should (looking-at "^add")))

   (save-excursion
     (coffee-beginning-of-defun)
     (looking-at "^Foo"))

   (coffee-end-of-block)
   (should (eobp))))

(ert-deftest move-defun-commands-with-toplevel-assignments ()
  "Move defun commands with toplevel assignmentsprototype access"

  (with-coffee-temp-buffer
   "
Foo::bar::baz = (apple, orange) ->
  apple + orange

value = 10
"
   (forward-cursor-on "Foo")
   (coffee-end-of-block)

   (save-excursion
     (forward-line 1)
     (should (looking-at "^value")))

   (save-excursion
     (coffee-beginning-of-defun)
     (looking-at "^Foo"))

   (coffee-end-of-block)
   (should (eobp))))

;;; coffee-command.el end here
