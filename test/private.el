;;; private.el --- Test for private functions of coffee-mode.el

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

(ert-deftest line-is-comment ()
  ""
  (with-coffee-temp-buffer
    "
# foo = 10
bar = 10
"
    (forward-cursor-on "foo")
    (should (coffee-line-is-comment))

    (forward-cursor-on "bar")
    (should-not (coffee-line-is-comment))))

(ert-deftest previous-line-comment ()
  ""
  (with-coffee-temp-buffer
    "
# foo = 10
bar = 10
baz = 20
"
    (forward-cursor-on "bar")
    (should (coffee-previous-line-is-comment))

    (forward-cursor-on "baz")
    (should-not (coffee-previous-line-is-comment))))

;;; private.el end here
