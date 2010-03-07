CoffeeScript Major Mode
=======================

An Emacs major mode for [CoffeeScript][cs], unfancy JavaScript.

Provides syntax highlight by way of font-lock, basic indentation, and
a few cute commands.

![Screenshot](http://img.skitch.com/20100307-qmcr7kij6fx7qmsx6w12dgfs2x.png)

## Installation

In your shell:

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/defunkt/coffee-mode.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
    (require 'coffee-mode)

`coffe-mode` will be enabled automatically for any files ending in
".coffee".

## Commands

### coffee-compile-buffer

Compiles the current buffer to JavaScript using the command specified
by the `coffee-command` variable and opens the contents in a new
buffer using your JavaScript mode of choice. The JavaScript mode is
determined by the `coffee-js-mode` variable and defaults to `js2-mode`.

Bind it:

    (define-key coffee-mode-map [(meta r)] 'coffee-compile-buffer)

### coffee-compiler-region

Compiles the selected region to JavaScript using the same
configuration variables as `coffee-compile-buffer`.

Bind it:

    (define-key coffee-mode-map [(meta R)] 'coffee-compile-region)

### coffee-repl

Starts a repl in a new buffer using `coffee-command`.

## Hooks

### coffee-mode-hook

Naturally. Example:

    (defun coffee-custom ()
      "coffee-mode-hook"
      (setq coffee-command "~/dev/coffee))

    (add-hook coffee-mode-hook
      '(lambda() (coffee-custom)))

## Thanks

* <http://xahlee.org/emacs/elisp_syntax_coloring.html> for instructions.
* Jason Blevins for the guidance his markdown-mode.el gave.

## Bugs

It's tested on Aquamacs 1.9 (Emacs 22) for OS X Snow Leopard so it may
not work on your environment. Please file a bug at
<http://github.com/defunkt/coffee-mode/issues> and maybe we can fix
the problem.

[cs]: http://jashkenas.github.com/coffee-script/
