;;; coffee-private.el --- Test for private functions of coffee-mode.el

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

(ert-deftest coffee-command-compile-without-output-argument ()
  "`coffee-command-compile' without output argument"
  (let ((coffee-command "coffee"))
    (let* ((got (coffee-command-compile "foo.coffee"))
           (output-dir (expand-file-name default-directory))
           (expected (format "coffee -c -o %s %s"
                             output-dir (concat output-dir "foo.coffee"))))
      (should (string= got expected)))))

(ert-deftest coffee-command-compile-with-output-argument ()
  "`coffee-command-compile' with output argument"
  (let ((coffee-command "coffee"))
    (let* ((got (coffee-command-compile "foo.coffee" "bar.js"))
           (output-dir (expand-file-name default-directory))
           (expected (format "coffee -c -j bar.js -o %s %s"
                             output-dir (concat output-dir "foo.coffee"))))
      (should (string= got expected)))))

(ert-deftest coffee-compiled-file-name ()
  "`coffee-compiled-file-name' with file name"
  (let ((coffee-js-directory ""))
    (let ((got (coffee-compiled-file-name "foo.coffee"))
          (expected (concat (expand-file-name default-directory) "foo.js")))
      (should (string= got expected)))))

(ert-deftest coffee-compiled-file-name-with-output-directory-abs ()
  "`coffee-compiled-file-name' with absolute `coffee-js-directory'"
  (let ((coffee-js-directory "/foo/bar"))
    (let ((got (coffee-compiled-file-name "baz.coffee"))
          (expected "/foo/bar/baz.js"))
      (should (string= got expected)))))

(ert-deftest coffee-compiled-file-name-with-output-directory-abs-slash ()
  "`coffee-compiled-file-name' with absolute slash `coffee-js-directory'"
  (let ((coffee-js-directory "/foo/bar/"))
    (let ((got (coffee-compiled-file-name "baz.coffee"))
          (expected "/foo/bar/baz.js"))
      (should (string= got expected)))))

(ert-deftest coffee-compiled-file-name-with-output-directory-rel ()
  "`coffee-compiled-file-name' with relative `coffee-js-directory'"
  (let ((coffee-js-directory "foo/bar"))
    (let ((got (coffee-compiled-file-name "baz.coffee"))
          (expected (concat (expand-file-name default-directory)
                            "foo/bar/baz.js")))
      (should (string= got expected)))))

;;
;; Comment
;;

(ert-deftest previous-line-is-single-line-comment ()
  ""
  (with-coffee-temp-buffer
    "
# foo = 10
bar = 10
baz = 20
"
    (forward-cursor-on "bar")
    (should (coffee-previous-line-is-single-line-comment))

    (forward-cursor-on "baz")
    (should-not (coffee-previous-line-is-single-line-comment))))

(ert-deftest previous-line-is-not-single-line-comment ()
  ""
  (with-coffee-temp-buffer
    "
###
#bar = 10
baz = 20
"
    (forward-cursor-on "bar")
    (should-not (coffee-previous-line-is-single-line-comment))

    (forward-cursor-on "baz")
    (should (coffee-previous-line-is-single-line-comment))))

;;
;; want new line
;;

(ert-deftest wants-indent-within-object ()
  "want indent within object"
  (with-coffee-temp-buffer
    "
a = {
  foo: 'bar'
}
"
    (forward-cursor-on "foo")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-within-array ()
  "want indent within array"
  (with-coffee-temp-buffer
    "
a = [
  'apple'
]
"
    (forward-cursor-on "apple")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-within-function ()
  "want indent within function"
  (with-coffee-temp-buffer
    "
foo = (arg) ->
  arg + 10
"
    (forward-cursor-on "10")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-within-class ()
  "want indent within class"
  (with-coffee-temp-buffer
    "
class Foo
  @bar = 10
"
    (forward-cursor-on "10")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-within-for ()
  "want indent within for"
  (with-coffee-temp-buffer
    "
for i in ['a', 'b', 'c']
  print i
"
    (forward-cursor-on "print")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-within-if ()
  "want indent within if"
  (with-coffee-temp-buffer
    "
if true
  foo = bar
"
    (forward-cursor-on "foo")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-within-try ()
  "want indent within try"
  (with-coffee-temp-buffer
    "
try
  undefined_func
catch error
  print foo
"
    (forward-cursor-on "undefined_func")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-within-while ()
  "want indent within while"
  (with-coffee-temp-buffer
    "
while bar -= 1
  foo -= 1
"
    (forward-cursor-on "foo")
    (should (coffee-line-wants-indent))))

(ert-deftest wants-indent-with-multiple-newlines ()
  "want indent with multiple newlines"
  (with-coffee-temp-buffer
    "
class Foo



  @bar = 1
"
    (forward-cursor-on "1")
    (should (coffee-line-wants-indent))))

;;
;; move command utility
;;

(ert-deftest skip-line-predicate ()
  "skip line predicate"
  (with-coffee-temp-buffer
    "
# comment

end
"
    (forward-cursor-on "comment")
    (should (coffee-skip-line-p))

    (forward-line 1)
    (should (coffee-skip-line-p))

    (forward-cursor-on "end")
    (should-not (coffee-skip-line-p))))

(ert-deftest skip-forward-line ()
  "skip line if line is comment or empty"
  (with-coffee-temp-buffer
    "
# foo
# bar
# baz
boo
end
"
    (goto-char (point-min))
    (coffee-skip-forward-lines -1)
    (should (= (point) (point-min)))

    (forward-cursor-on "foo")
    (coffee-skip-forward-lines +1)

    (should (looking-at "^boo"))

    (goto-char (point-max))
    (coffee-skip-forward-lines +1)
    (should (= (point) (point-max)))))

;;; coffee-private.el end here
