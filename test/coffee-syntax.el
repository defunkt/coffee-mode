;;; coffee-syntax.el --- Test for syntax of coffee-mode.el

;; Copyright (C) 2016 by Syohei YOSHIDA

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
;; Treat variables with "_" as word delimiters
;;
(ert-deftest treat-underscores-within-variables-as-symbols ()
  "Treat '_' within variable names as word delimiters"
  (with-coffee-temp-buffer
    "foo_bar"
    (forward-word 1)
    (forward-cursor-on "_")
    (should (not (eobp)))

    (forward-word 1)
    (should (eobp))

    (backward-word 1)
    (should (not (bobp)))
    (forward-cursor-on "bar")
    (backward-cursor-on "foo")

    (backward-word 1)
    (forward-cursor-on "foo")
    (should (bobp))))

;;
;; #219 Invalid slash property
;;
(ert-deftest slash-syntax-property ()
  "`/' is not treat as close paren"
  (with-coffee-temp-buffer
    "( / )"
    (forward-sexp 1)
    (should (eobp))

    (backward-sexp 1)
    (should (bobp))

    (forward-cursor-on "/")
    (backward-up-list)
    (should (bobp))))

;;; coffee-syntax.el end here
