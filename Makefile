.PHONY : test test-command test-imenu test-highlight test-syntax test-private

UNAME_S=$(shell uname -s)
ifeq ($(UNAME_S),Darwin)
	EMACS ?= /Applications/Emacs.app/Contents/MacOS/Emacs
else
	EMACS ?= emacs
endif

LOADPATH = -L .
LOAD_HELPER = -l test/test-helper.el

test:
	$(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/coffee-command.el \
		-l test/coffee-imenu.el -l test/coffee-highlight.el \
		-l test/coffee-private.el -l test/coffee-syntax.el \
		-f ert-run-tests-batch-and-exit

test-highlight:
	$(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/coffee-highlight.el \
		-f ert-run-tests-batch-and-exit

test-syntax:
	$(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/coffee-syntax.el \
		-f ert-run-tests-batch-and-exit

test-command:
	$(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/coffee-command.el \
		-f ert-run-tests-batch-and-exit

test-imenu:
	$(CASK) exec $(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/coffee-imenu.el \
		-f ert-run-tests-batch-and-exit

test-private:
	$(EMACS) -Q -batch $(LOADPATH) $(LOAD_HELPER) \
		-l test/coffee-private.el \
		-f ert-run-tests-batch-and-exit
