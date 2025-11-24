

TESTDIR=/tmp/test-dwin/
MELPAZOID_DIR=~/local/system/misc/melpazoid/

THIS_MAKEFILE := $(lastword $(MAKEFILE_LIST))
PACKAGE_DIR=$(shell realpath $(dir $(THIS_MAKEFILE))/..)

.PHONY: help
help:
	echo see Makefile

.PHONY: show
show:
	echo "package dir: $(PACKAGE_DIR)"

.PHONY: test
test:
	mkdir -p $(TESTDIR)/straight/repos
	ln -s $(PACKAGE_DIR)/etc/example-emacs-init/init.el $(TESTDIR)/init.el || true
	ln -s $(PACKAGE_DIR) $(TESTDIR)/straight/repos || true
	emacs --debug-init --init-directory $(TESTDIR)

.PHONY: dev-emacs
dev-emacs:
	emacs --debug-init --init-directory dev-emacs-init/

# beware: emacs --batch implies -q, so --init-directory is useless.
# - either use --batch -l ...  or [not --batch] --init-directory.


.PHONY: preview
preview:
	cd .. && go-grip -p 8100 README.md

check-melpazoid:
	RECIPE='(dwin :fetcher github :repo "lsth/dwin" :files (:defaults "etc/bin" "etc/_local-share-applications"))' \
        LOCAL_REPO="$(PACKAGE_DIR)" make -C "$(MELPAZOID_DIR)"
