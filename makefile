EMACS ?= emacs
EASK ?= eask

TESTHOME=/tmp/emacs

ci: build test

package:
	$(EASK) package

build: package
	$(EASK) install

test:
	@echo "Testing..."
	$(EASK) install --dev
	$(EASK) ert csharp-mode-tests.el

clean:
	$(EASK) clean-elc
	rm -rf dist
	rm -rf $(TESTHOME)

check-defuns:
	grep "^(defun " csharp-mode.el | sed -r "s/\(defun ([a-z0-9-]+) .*$$/\1/" | sort >/tmp/defuns.txt
	for line in `cat /tmp/defuns.txt` ; do echo -n "$$line: " ; grep "$$line" csharp-mode.el | grep -v defun | wc -l ; done >/tmp/use-count.txt
	grep " 0" /tmp/use-count.txt
