EMACS="$(shell which emacs)"
EMACS_CLI=$(EMACS) -Q -batch -L .
CASK=~/.cask/bin/cask
TESTHOME=/tmp/emacs

package: build
	$(CASK) package

build: test
	$(CASK) build

test: *.el
	rm -rf $(TESTHOME)
	mkdir -p $(TESTHOME)
	+ HOME=$(TESTHOME) $(EMACS_CLI) -l csharp-mode-tests.el -f ert-run-tests-batch-and-exit

clean:
	$(CASK) clean-elc
	rm -rf dist

check-defuns:
	grep "^(defun " csharp-mode.el | sed -r "s/\(defun ([a-z0-9-]+) .*$$/\1/" | sort >/tmp/defuns.txt
	for line in `cat /tmp/defuns.txt` ; do echo -n "$$line: " ; grep "$$line" csharp-mode.el | grep -v defun | wc -l ; done >/tmp/use-count.txt
	grep " 0" /tmp/use-count.txt

