CoffeeScript Major Mode
=======================

An Emacs major mode for [CoffeeScript][cs], unfancy JavaScript.

Provides syntax highlighting, indentation support, and a few cute
commands.

![Screenshot](http://img.skitch.com/20100307-qmcr7kij6fx7qmsx6w12dgfs2x.png)

## Installation

In your shell:

    $ cd ~/.emacs.d/vendor
    $ git clone git://github.com/defunkt/coffee-mode.git

In your emacs config:

    (add-to-list 'load-path "~/.emacs.d/vendor/coffee-mode")
    (require 'coffee-mode)

`coffee-mode` will be enabled automatically for any files ending in
".coffee" or named "Cakefile".

## Indentation

### Configuring

Lines are indented according to the `tab-width` variable. If you're
like me, you probably have this set in your Emacs config globally:

    (setq-default tab-width 4)

Well, idiomatic CoffeeScript uses two spaces. We can set our
`tab-width` to two for `coffee-mode` using the `coffee-mode-hook`:

    (defun coffee-custom ()
      "coffee-mode-hook"
     (set (make-local-variable 'tab-width) 2))

    (add-hook coffee-mode-hook
      '(lambda() (coffee-custom)))

Another example of this hook is given further down.

### TAB Theory

When you press `TAB`, indent the line unless doing so would make the
current line more than two indentation levels deepers than the
previous line. If that's the case, remove all indentation.

Consider this code, with point at the position indicated by the
caret:

    line1()
      line2()
      line3()
         ^

Pressing `TAB` will produce the following code:

    line1()
      line2()
        line3()
           ^

Pressing `TAB` again will produce this code:

    line1()
      line2()
    line3()
       ^

And so on. I think this is a pretty good way of getting decent
indentation with a whitespace-sensitive

### Newline and Indent

As for indentation after newlines, given this code and cursor
position:

    line1()
      line2()
      line3()
            ^

Pressing `RET` would insert a newline and place our cursor at the
following position:

    line1()
      line2()
      line3()

      ^

In other words, the level of indentation is maintained. This
applies to comments as well. Combined with the `TAB` you should be
able to get things where you want them pretty easily.

### Indenters

`class`, `for`, `if`, and possibly other keywords cause the next line
to be indented automatically, however.

For example, given this code and cursor position::

    class Animal
                ^

Pressing enter would produce the following:

    class Animal

      ^

That is, indented a column deeper.

This also applies to lines ending in `->`, `=>`, `{`, `[`, and
possibly more characters.

So this code and cursor position:

    $('#demo').click ->
                       ^

On enter would produce this:

    $('#demo').click ->

      ^

Pretty slick.

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
      (set (make-local-variable 'tab-width) 2)
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
