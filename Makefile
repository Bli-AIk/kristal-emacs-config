EMACS ?= emacs
TESTS ?= $(wildcard test/*-test.el)
TEST_LOADS = $(if $(filter test/fumos-eval-test.el,$(TESTS)),\
  $(filter-out test/fumos-repl-test.el,$(TESTS)),$(TESTS))
LOAD_PATH = -L vendor/fennel-mode -L lisp -L test

.PHONY: test test-upstream test-doom test-installer testall clean

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/test-helper.el \
	  $(foreach test,$(TEST_LOADS),-l $(test)) \
	  -f ert-run-tests-batch-and-exit

test-upstream:
	$(MAKE) -C vendor/fennel-mode testall EMACS=$(EMACS)

test-doom:
	sh test/run-doom-tests.sh

test-installer:
	sh test/install-fennel-ls-test.sh

testall: test test-upstream test-doom test-installer

clean:
	find lisp test -name '*.elc' -delete 2>/dev/null || true
