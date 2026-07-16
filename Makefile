EMACS ?= emacs
VANILLA_TESTS = \
	test/harness-test.el \
	test/vendor-test.el \
	test/fake-fumos-server-test.el \
	test/fumos-instance-test.el \
	test/fumos-project-test.el \
	test/fumos-eglot-test.el \
	test/fumos-repl-test.el \
	test/fumos-eval-test.el
TESTS ?= $(VANILLA_TESTS)
LOAD_PATH = -L vendor/fennel-mode -L lisp -L test

.PHONY: test test-upstream test-doom test-installer testall clean

test:
	$(EMACS) -Q --batch $(LOAD_PATH) -l test/test-helper.el \
	  $(foreach test,$(TESTS),-l $(test)) \
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
