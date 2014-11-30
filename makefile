VERSION:=0.8.7
PACKAGE_SHORTNAME=csharp-mode
PACKAGE_NAME:=$(PACKAGE_SHORTNAME)-$(VERSION)
PACKAGE_DIR:=/tmp/$(PACKAGE_NAME)

package: $(PACKAGE_DIR)
	tar cvf ../$(PACKAGE_NAME).tar --exclude="*#" --exclude="*~" -C $(PACKAGE_DIR)/.. $(PACKAGE_NAME)

$(PACKAGE_DIR):
	mkdir $@
	cp -r ../$(PACKAGE_SHORTNAME)/* $@
	sed -re "s/VERSION/$(VERSION)/" $@/$(PACKAGE_NAME)-package-template.el > $@/$(PACKAGE_NAME)-pkg.el

clean:
	rm -f ../$(PACKAGE_NAME).tar
	rm -rf $(PACKAGE_DIR)

# end
