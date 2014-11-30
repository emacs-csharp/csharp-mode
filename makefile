#VERSION:=0.8.7
VERSION=$(shell grep ";; Version " csharp-mode.el | cut -d ":" -f2 | cut -c2-)
PACKAGE_SHORTNAME=csharp-mode
PACKAGE_NAME:=$(PACKAGE_SHORTNAME)-$(VERSION)
#PACKAGE_DIR:=./.tmp/$(PACKAGE_NAME)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

EMACS=$(shell which emacs) -Q -batch -L .
ELS = csharp-mode.el csharp-mode-tests.el
ELCS = $(ELS:.el=.elc)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir -p $@
	cp -r ../$(PACKAGE_SHORTNAME)/* $@
	sed -re "s/VERSION/$(VERSION)/" $@/$(PACKAGE_SHORTNAME)-package-template.el > $@/$(PACKAGE_NAME)-pkg.el

test:
	+ $(EMACS) -l csharp-mode-tests.el -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -f batch-byte-compile $<

#all: $(ELCS) test package
all: test package

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)
	rm -rf $ELCS

# end
