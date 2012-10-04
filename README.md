CoffeeScript Major Mode
=======================

An Emacs major mode for [CoffeeScript][cs] and [IcedCoffeeScript][ics],
unfancy JavaScript.

Provides syntax highlighting, indentation support, imenu support,
a menu bar, and a few cute commands.

![Screenshot](http://img.skitch.com/20100308-fcr622c95ibey4m474d5m1m1qt.png)

## Installation

Install from the [GNU Emacs Lisp Package Archive][elpa].

[coffee-mode used to offer automatic deletion of trailing whitespace.
This is now left to whitespace-mode. See its documentation for full
details, but as a hint, configure:

    (setq whitespace-action '(auto-cleanup)) ;; automatically clean up bad whitespace
    (setq whitespace-style '(trailing space-before-tab indentation empty space-after-tab)) ;; only show bad whitespace

Then turn on whitespace-mode, or global-whitespace-mode.]

## imenu

If you're using imenu, `coffee-mode` should work just fine. This
means users of [textmate.el][tm] will find that `⇧⌘T`
(`textmate-go-to-symbol`) mostly works as expected.

If you're not using imenu check out [this page][im] or textmate.el for
a really awesome way to jump quickly to a function's definition in a
file.

## Commands

If you have `easymenu` you can get to any of these commands from the
menu bar:

![coffee-mode menu bar](http://img.skitch.com/20100308-tt5yn51h2jww2pmjqaawed6eq8.png)

## Bugs

Please file bugs at <http://github.com/defunkt/coffee-mode/issues>

[cs]: http://jashkenas.github.com/coffee-script/
[ics]: http://maxtaco.github.com/coffee-script/
[tm]: http://github.com/defunkt/textmate.el
[im]: http://chopmo.blogspot.com/2008/09/quickly-jumping-to-symbols.html
[elpa]: http://elpa.gnu.org/
