.PHONY : test test-command test-imenu test-highlight test-syntax test-private

EMACS ?= emacs
LOADPATH ?= -L . -L test

test:
	$(EMACS) -Q -batch $(LOADPATH) -l test/command.el -l test/coffee-imenu.el \
		-l test/highlight.el -l test/private.el -l test/syntax.el \
		-f ert-run-tests-batch-and-exit

test-highlight:
	$(EMACS) -Q -batch $(LOADPATH) -l test/highlight.el -f ert-run-tests-batch-and-exit

test-syntax:
	$(EMACS) -Q -batch $(LOADPATH) -l test/syntax.el -f ert-run-tests-batch-and-exit

test-command:
	$(EMACS) -Q -batch $(LOADPATH) -l test/command.el -f ert-run-tests-batch-and-exit

test-imenu:
	$(EMACS) -Q -batch $(LOADPATH) -l test/coffee-imenu.el -f ert-run-tests-batch-and-exit

test-private:
	$(EMACS) -Q -batch $(LOADPATH) -l test/private.el -f ert-run-tests-batch-and-exit
