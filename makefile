VERSION=$(shell grep -a ";; Version " csharp-mode.el | cut -d ":" -f2 | cut -c2-)
PACKAGE_SHORTNAME=csharp-mode
PACKAGE_NAME:=$(PACKAGE_SHORTNAME)-$(VERSION)
PACKAGE_DIR:=./.tmp/$(PACKAGE_NAME)
#PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

EMACS="$(shell which emacs)" -Q -batch -L .
ELS = csharp-mode.el csharp-mode-tests.el
ELCS = $(ELS:.el=.elc)

all: $(ELCS) test package

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" --exclude="*tests*" --exclude="test-files" --exclude "*-pkg.el.template*" --exclude="makefile" --exclude="run-travis-ci.sh" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir -p $@
	cp -r ../$(PACKAGE_SHORTNAME)/* $@
	sed -re "s/VERSION/$(VERSION)/" $@/$(PACKAGE_SHORTNAME)-pkg.el.template > $@/$(PACKAGE_SHORTNAME)-pkg.el

test:
	+ $(EMACS) -l csharp-mode-tests.el -f ert-run-tests-batch-and-exit

%.elc: %.el
	$(EMACS) -f batch-byte-compile $<

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)
	rm -rf $(ELCS)

check-defuns:
	grep "^(defun " csharp-mode.el | sed -r "s/\(defun ([a-z0-9-]+) .*$$/\1/" | sort >/tmp/defuns.txt
	for line in `cat /tmp/defuns.txt` ; do echo -n "$$line: " ; grep "$$line" csharp-mode.el | grep -v defun | wc -l ; done >/tmp/use-count.txt
	grep " 0" /tmp/use-count.txt


# end
