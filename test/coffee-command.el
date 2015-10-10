;;; coffee-command.el --- Test for commands of coffee-mode.el

;; Copyright (C) 2014 by Syohei YOSHIDA

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
;; dedent(backspace behavior)
;;

(ert-deftest dedent-command-multiple-of-coffee-tab-width ()
  "dedent - current column is multiple of coffee-tab-width"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
line1()
  line2()
"
      (forward-cursor-on "line2")
      (call-interactively 'coffee-dedent-line-backspace)
      (should (= (current-column) 0)))))

(ert-deftest dedent-command-not-multiple-of-coffee-tab-width ()
  "dedent - current column is not multiple of coffee-tab-width"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
line1()
   line2()
"
      (forward-cursor-on "line2")
      (call-interactively 'coffee-dedent-line-backspace)
      (should (= (current-column) 2)))))

(ert-deftest dedent-command-at-not-beginning-of-line ()
  "dedent - current position is not beginning of line"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "foo"
      (goto-char (point-max))
      (call-interactively 'coffee-dedent-line-backspace)
      (should (= (current-column) 2))
      (should (string= (buffer-string) "fo")))))

(ert-deftest dedent-command-with-numeric-prefix ()
  "dedent - with numeric prefix"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
line1()
   line2()
"
      (forward-cursor-on "line2")
      (coffee-dedent-line-backspace 2)
      (should (= (current-column) 1)))))

(ert-deftest dedent-command-with-electric-pair-mode ()
  "dedent - with electric-pair-mode"
  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
line1()
  line2()
"
      (electric-pair-mode +1)
      (forward-cursor-on "line2")
      (call-interactively 'coffee-dedent-line-backspace)
      (should (= (current-column) 0)))

    (with-coffee-temp-buffer
      "
line1()
   line2()
"
      (electric-pair-mode +1)
      (forward-cursor-on "line2")
      (call-interactively 'coffee-dedent-line-backspace)
      (should (= (current-column) 2)))))

;;
;; indent for else line
;;
(ert-deftest indent-if-else-else-line ()
  "Indent for `else' line"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
for a in [1]
  for b in [2]
    if true
      a + b
else
"
     (let (if-indent)
       (forward-cursor-on "if")
       (setq if-indent (current-indentation))
       (forward-cursor-on "else")
       (call-interactively 'indent-for-tab-command)
       (should (= if-indent (current-indentation)))))

    (let ((coffee-tab-width 2))
      (with-coffee-temp-buffer
        "
for a in [1]
  for b in [2]
    if true
      a + b
      else
"
        (let (if-indent)
          (forward-cursor-on "if")
          (setq if-indent (current-indentation))
          (forward-cursor-on "else")
          (call-interactively 'indent-for-tab-command)
          (should (= if-indent (current-indentation))))))))

(ert-deftest indent-if-else-else-if-line ()
  "Indent for `else-if' line"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
for a in [1]
  for b in [2]
    if true
      a + b
else if
"
      (let (if-indent)
        (forward-cursor-on "if")
        (setq if-indent (current-indentation))
        (forward-cursor-on "else")
        (call-interactively 'indent-for-tab-command)
        (should (= if-indent (current-indentation)))))))

(ert-deftest indent-if-else-not-indent ()
  "Don't indent case for `else' line indent"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
if true
  a + b
else
"
      (forward-cursor-on "else")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 0)))))

(ert-deftest indent-if-else-not-indent-but-moving-cursor ()
  "Don't indent but moving cursor for if-else block"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
  if true
    a + b
  else
"
      (let (if-indent)
        (forward-cursor-on "if")
        (setq if-indent (current-indentation))
        (forward-cursor-on "else")
        (goto-char (line-beginning-position))
        (call-interactively 'indent-for-tab-command)
        (should (= if-indent (current-column)))))))

(ert-deftest indent-if-else-nested ()
  "Indent for nested if-else blocks"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
if a
  for b in [1]
    if c
      true
    else if b
else
"
      (goto-char (point-max))
      (backward-cursor-on "else")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 0))))

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
    if 1 == 1
      for name in ['taro', 'jiro', 'saburo']
        if 2 == 2
          true
        else
      if 3 == 3
        true
      else if
        false
        for name in ['hoge']
          if true
             1 + 2
else if
"
      (goto-char (point-max))
      (backward-cursor-on "else")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 6))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 10))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4)))))

(ert-deftest indent-if-else-between-functions ()
  "Don't indent size same as if-else block in another function"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
foo = () ->
  if a
     true

bar = () ->
  for b in [10]
    if b == 10
      false
else
"
      (forward-cursor-on "else")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4)))))

(ert-deftest indent-if-else-with-closed-if-else-block ()
  "Don't indent level same as already closed if-else block"

  (with-coffee-temp-buffer
    "
for a in [1]
  if true
    a
  for b in [2]
    if true
      a + b
else
"
    (let (if-indent)
      (forward-cursor-on "if" 2)
      (setq if-indent (current-indentation))
      (forward-cursor-on "else")
      (call-interactively 'indent-for-tab-command)
      (should (= if-indent (current-indentation)))
      (call-interactively 'indent-for-tab-command)
      (should (= if-indent (current-indentation)))))

  (with-coffee-temp-buffer
    "
for a in [1]
  if true
    a
  else if true
    a
  for b in [2]
    if true
      a + b
else
"
    (let (if-indent)
      (forward-cursor-on "if" 3)
      (setq if-indent (current-indentation))
      (forward-cursor-on "else")
      (call-interactively 'indent-for-tab-command)
      (should (= if-indent (current-indentation)))
      (call-interactively 'indent-for-tab-command)
      (should (= if-indent (current-indentation))))))

(ert-deftest indent-if-else-in-string ()
  "Indent for `else' line in string"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
\"\"\"
if true
  a + b
else
\"\"\"
"
      (forward-cursor-on "else")
      (call-interactively 'indent-for-tab-command)
      (should-not (= (current-indentation) 0))
      (should (= (current-indentation) coffee-tab-width)))))

;;
;; indent for try-catch-finally block
;;
(ert-deftest indent-try-catch-cactch-line ()
  "Indent for `catch' line of try-catch"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
for a in [1]
  for b in [2]
    try
      raise_exception(a, b)
catch
"
      (let (try-indent)
        (forward-cursor-on "try")
        (setq try-indent (current-indentation))
        (forward-cursor-on "catch")
        (call-interactively 'indent-for-tab-command)
        (should (= try-indent (current-indentation)))))))

(ert-deftest indent-try-catch-finally-line ()
  "Indent for `finally' line of try-catch"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
for a in [1]
  for b in [2]
    try
      raise_exception(a, b)
finally
"
      (let (try-indent)
        (forward-cursor-on "try")
        (setq try-indent (current-indentation))
        (forward-cursor-on "finally")
        (call-interactively 'indent-for-tab-command)
        (should (= try-indent (current-indentation)))))))

(ert-deftest indent-try-catch-not-indent-but-moving-cursor ()
  "Don't indent but moving cursor for try-catch block"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
  try
     raise_exception(1, '2')
  catch
"
      (let (try-indent)
        (forward-cursor-on "try")
        (setq try-indent (current-indentation))
        (forward-cursor-on "catch")
        (goto-char (line-beginning-position))
        (call-interactively 'indent-for-tab-command)
        (should (= try-indent (current-column)))))))

(ert-deftest indent-try-catch-nested ()
  "Indent for nested try-catch blocks"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
try
  for b in [1]
    try c
      raise_exception
    catch error
      console.log error
finally
"
      (goto-char (point-max))
      (backward-cursor-on "finally")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 0))))

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
    try
      for name in ['taro', 'jiro', 'saburo']
        try
          raise_exception1
        catch error
          console.log 'dummy'
      try
        raise_exception2
      catch error2
        for name in ['hoge']
          try
             raise_exception3
finally
"
      (goto-char (point-max))
      (backward-cursor-on "finally")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 6))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 10))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4)))))

(ert-deftest indent-try-catch-between-functions ()
  "Don't indent size same as try-catch block in another function"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
foo = () ->
  try
    raise_some_exception
  catch error
    console.log error

bar = () ->
  for i in [1, 2, 3]
    try
      raise_some_exception2
finally
"
      (forward-cursor-on "finally")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4))
      (call-interactively 'indent-for-tab-command)
      (should (= (current-indentation) 4)))))

(ert-deftest indent-try-catch-with-closed-block ()
  "Indent try-catch block with already closed try-catch block"

  (with-coffee-temp-buffer
    "
for a in [1]
  try
    raise_exception1
  finally
    die 'I am dying'
  for b in [2]
    try
      raise_exception2
catch
"
    (let (try-indent)
      (forward-cursor-on "try" 2)
      (setq try-indent (current-indentation))
      (forward-cursor-on "catch")
      (call-interactively 'indent-for-tab-command)
      (should (= try-indent (current-indentation)))
      (call-interactively 'indent-for-tab-command)
      (should (= try-indent (current-indentation))))))

(ert-deftest first-line-indentation ()
  "First line indentation"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "    if true
console.log 'foo'
"
      (goto-char (point-min))
      (let ((first-line-indentation (current-indentation)))
        (forward-line 1)
        (call-interactively 'indent-for-tab-command)
        (should (= (current-indentation) 2))
        (call-interactively 'indent-for-tab-command)
        (should (= (current-indentation) 4))
        (call-interactively 'indent-for-tab-command)
        (should (= (current-indentation) 6))
        (call-interactively 'indent-for-tab-command)
        (should (= (current-indentation) 0))))))

;;
;; enable coffee-indent-tabs-mode
;;

(ert-deftest inserting-tab ()
  "inserting tab when `coffee-indent-tabs-mode' is enable"

  (let ((coffee-tab-width 8)
        (coffee-indent-tabs-mode t))
    (with-coffee-temp-buffer
      "
line1()
line2()
"
      (should coffee-indent-tabs-mode)
      (forward-cursor-on "line2")
      (call-interactively 'indent-for-tab-command)
      (should (= (current-column) 8))
      (should (looking-back "^\t"))

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

;; #239
(ert-deftest newline-and-indent-twice-issue ()
  "indent-new-line twice"
  (with-coffee-temp-buffer
    "
if true
"
    (forward-cursor-on "if")
    (goto-char (line-end-position))
    (coffee-newline-and-indent)
    (coffee-newline-and-indent)
    (should (= (current-indentation) coffee-tab-width))))

;; #239
(ert-deftest newline-and-indent-start-of-block ()
  "Don't insert indentation after block"
  (with-coffee-temp-buffer
    "
if true
  if true
"
    (let ((coffee-tab-width 2))
      (goto-char (point-max))
      (backward-cursor-on "if")
      (let ((indent (current-indentation)))
        (coffee-newline-and-indent)
        (should (= (current-indentation) indent))))))

;; #239
(ert-deftest newline-and-indent-from-bol ()
  "newline-and-indent from beginning of line"
  (with-coffee-temp-buffer
    "
if true
  if false

"
    (let ((coffee-tab-width 2))
      (forward-cursor-on "false")
      (goto-char (line-beginning-position))
      (coffee-newline-and-indent)
      (should (= (current-indentation) coffee-tab-width)))))

(ert-deftest newline-and-indent-not-additional-indent ()
  "Don't insert indentation after normal line"
  (with-coffee-temp-buffer
    "
if true
  foo = hoge

"
    (let ((coffee-tab-width 2))
      (forward-cursor-on "foo")
      (goto-char (line-end-position))
      (coffee-newline-and-indent)
      (should (= (current-indentation) coffee-tab-width)))))

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
;; indent region
;;

(ert-deftest indent-region-zero-indent ()
  "Indent region for no indentation case"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
    aa = 10
    bb = 20
    cc = 30
"
      (coffee-indent-region (point-min) (point-max))

      (goto-char (point-min))

      (forward-line 1)
      (should (= (current-indentation) 0))

      (forward-line 1)
      (should (= (current-indentation) 0))

      (forward-line 1)
      (should (= (current-indentation) 0)))))

(ert-deftest indent-region-wants-indent ()
  "Indent region for indent case"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
if true
foo
"
      (coffee-indent-region (point-min) (point-max))

      (goto-char (point-min))
      (forward-cursor-on "foo")
      (should (= (current-indentation) coffee-tab-width)))))

(ert-deftest indent-region-wants-indent-nest ()
  "Indent region for nested indent case"

  (let ((coffee-tab-width 2))
    (with-coffee-temp-buffer
      "
if true
      foo
          unless false
                 bar
"
      (coffee-indent-region (point-min) (point-max))

      (goto-char (point-min))
      (forward-cursor-on "foo")
      (should (= (current-indentation) coffee-tab-width))

      (forward-cursor-on "unless")
      (should (= (current-indentation) coffee-tab-width))

      (let ((cur-block-indent (current-indentation)))
        (forward-cursor-on "bar")
        (should (= (current-indentation) (+ cur-block-indent coffee-tab-width)))))))

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

(ert-deftest regression-test-270 ()
  "Regression test for #270"

  (with-coffee-temp-buffer
   "
a = 10
b = 20
"
   (forward-cursor-on "a")
   (call-interactively 'set-mark-command)
   (goto-char (point-max))
   (call-interactively 'coffee-comment-dwim)
   (should (not mark-active))))

(ert-deftest indent-same-as-python-mode ()
  "Move defun commands with toplevel assignmentsprototype access"

  (with-coffee-temp-buffer
   "
if 1 == 1
  if 2 == 2
    if 3 == 3
      if 4 == 4
        if 5 == 5
foo
"
   (let ((coffee-tab-width 2)
         (coffee-indent-like-python-mode t))
     (forward-cursor-on "foo")
     (call-interactively 'indent-for-tab-command)
     (let ((prev-indent (save-excursion
                          (forward-line -1)
                          (current-indentation))))
       (should (= (current-indentation) (+ prev-indent coffee-tab-width)))
       (let ((curindent (current-indentation)))
         (call-interactively 'beginning-of-line)
         (call-interactively 'delete-horizontal-space)
         (call-interactively 'coffee-indent-line)
         (call-interactively 'coffee-indent-line)
         (should (= (current-indentation) (- curindent coffee-tab-width)))

         ;; wrap around
         (call-interactively 'beginning-of-line)
         (call-interactively 'delete-horizontal-space)
         (call-interactively 'coffee-indent-line)
         (call-interactively 'coffee-indent-line)
         (call-interactively 'coffee-indent-line)
         (call-interactively 'coffee-indent-line)
         (call-interactively 'coffee-indent-line)
         (call-interactively 'coffee-indent-line)
         (call-interactively 'coffee-indent-line)
         (should (= (current-indentation) curindent))))))

  (with-coffee-temp-buffer
   "
if 1 == 1
  if 2 == 2
    a = 10
foo
"
   (let ((coffee-tab-width 2)
         (coffee-indent-like-python-mode t))
     (forward-cursor-on "foo")
     (call-interactively 'indent-for-tab-command)
     (let ((prev-indent (save-excursion
                          (forward-line -1)
                          (current-indentation))))
       (should (= prev-indent (current-indentation)))))))

;;; coffee-command.el end here
