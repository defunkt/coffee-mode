;;; command.el --- Test for commands of coffee-mode.el

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

;;; command.el end here
