

TESTDIR=/tmp/test-dwin/
TESTDIR28=/tmp/test-dwin-emacs28/
TEST_PACKAGE_DIR=/tmp/test-dwin-repo/
MELPAZOID_DIR=~/local/system/misc/melpazoid/

THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
PACKAGE_DIR=$(shell realpath $(dir $(THIS_MAKEFILE))/..)
TEST_EMACS?=emacs
TEST_EMACS28?=rare-emacs-28.1

.PHONY: help
help:
	echo see Makefile

.PHONY: show
show:
	echo "package dir: $(PACKAGE_DIR)"

.PHONY: test
test:
	@echo "test emacs is: $(TEST_EMACS), $(shell $(TEST_EMACS) --version)"
	mkdir -p $(TESTDIR)/straight/repos
	ln -s $(PACKAGE_DIR)/etc/example-emacs-inits/02-dwin-via-straight/init.el $(TESTDIR)/init.el || true
	ln -s $(PACKAGE_DIR) $(TESTDIR)/straight/repos || true
	$(TEST_EMACS) --debug-init --init-directory $(TESTDIR)

test-emacs28:
	@echo "test emacs is: $(TEST_EMACS28), $(shell $(TEST_EMACS) --version)"
	mkdir -p $(TESTDIR28)/straight/repos
	ln -s $(PACKAGE_DIR)/etc/example-emacs-inits/04-dwin-via-straight-emacs-before-29.1/init.el $(TESTDIR28)/init.el || true
	ln -s $(PACKAGE_DIR) $(TESTDIR28)/straight/repos || true
	$(TEST_EMACS28) -Q --debug-init $(TESTDIR28)/init.el


.PHONY: dev-emacs
dev-emacs:
	emacs --debug-init --init-directory dev-emacs-init/

# beware: emacs --batch implies -q, so --init-directory is useless.
# - either use --batch -l ...  or [not --batch] --init-directory.


.PHONY: preview
preview:
	cd .. && go-grip -p 8100 README.md

check-melpazoid:
	rm -fr $(TEST_PACKAGE_DIR)
	./bin-dev/cp-nonignored-git-files $(PACKAGE_DIR) $(TEST_PACKAGE_DIR)
	RECIPE='(dwin :fetcher github :repo "lsth/dwin" :files (:defaults "etc/bin" "etc/_local-share-applications"))' \
        LOCAL_REPO="$(TEST_PACKAGE_DIR)" make -C "$(MELPAZOID_DIR)"
