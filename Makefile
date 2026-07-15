EMACS ?= emacs
TESTS ?= $(wildcard test/*-test.el)
LOAD_PATH = -L vendor/fennel-mode -L lisp -L test

.PHONY: test test-upstream test-doom testall clean

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/test-helper.el \
	  $(foreach test,$(TESTS),-l $(test)) \
	  -f ert-run-tests-batch-and-exit

test-upstream:
	$(MAKE) -C vendor/fennel-mode testall EMACS=$(EMACS)

test-doom:
	sh test/run-doom-tests.sh

testall: test test-upstream test-doom

clean:
	find lisp test -name '*.elc' -delete 2>/dev/null || true
