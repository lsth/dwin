

TESTDIR=/tmp/test-dwin/

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
