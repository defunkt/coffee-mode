;;; imenu.el --- Test for imenu of coffee-mode.el

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

(ert-deftest class-members ()
  "Creating class member indice"
  (with-coffee-temp-buffer
    "
class Person
  minus: (x, y) -> x - y
  print: =>
    print 'My name is ' + this.name + '.'
"
    (let ((got (coffee-imenu-create-index)))
      (should (= (length got) 2))
      (dolist (expected '("Person::minus" "Person::print"))
        (should (loop for index in got
                      for assign = (car index)
                      thereis (string= assign expected)))))))

(ert-deftest extended-class-members ()
  "Creating extended class member indice"
  (with-coffee-temp-buffer
    "
class Policeman extends Person
  constructor: (rank) ->
    @rank = rank
  print: ->
    print 'My name is ' + this.name + \" and I'm a \" + this.rank + '.'
"
    (let ((got (coffee-imenu-create-index)))
      (should (= (length got) 2))
      (dolist (expected '("Policeman::constructor" "Policeman::print"))
        (should (loop for index in got
                      for assign = (car index)
                      thereis (string= assign expected)))))))

(ert-deftest object-properties ()
  "Creating object property indice"
  (with-coffee-temp-buffer
    "
a =
  minus: (x, y) -> x - y
  block: ->
    print('potion')
"
    (let ((got (coffee-imenu-create-index)))
      (should (= (length got) 2))
      (dolist (expected '("minus" "block"))
        (should (loop for index in got
                      for assign = (car index)
                      thereis (string= assign expected)))))))

(ert-deftest class-members-and-object-properties ()
  "Creating class member and object property indice"
  (with-coffee-temp-buffer
    "
class Foo
  constructor: (rank) ->
    @rank = rank
  print: ->
    print 'My name is ' + this.name + \" and I'm a \" + this.rank + '.'

a =
  minus: (x, y) -> x - y
  block: ->
    print('potion')
"
    (let ((got (coffee-imenu-create-index)))
      (should (= (length got) 4))
      (dolist (expected '("Foo::constructor" "Foo::print" "a.minus" "a.block"))
        (should (loop for index in got
                      for assign = (car index)
                      thereis (string= assign expected)))))))

;;; imenu.el end here
