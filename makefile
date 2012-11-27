TAR=tar
TARFLAGS=-cf

pkg: inline-crypt.el inline-crypt-pkg.el README.org LICENSE.txt
	$(eval VERSION=$(shell egrep "[;]+ Version: [0-9a-zA-Z.]+" $< | cut -d ' ' -f 3))
	$(eval PKG_DIRNAME=inline-crypt-$(VERSION))
	echo $(PKG_DIRNAME)
	$(eval TAR_FILENAME=$(PKG_DIRNAME).tar)
	mkdir $(PKG_DIRNAME)
	cp -r $^ $(PKG_DIRNAME)
	mv $(PKG_DIRNAME)/README* $(PKG_DIRNAME)/README
	$(TAR) $(TARFLAGS) $(TAR_FILENAME) $(PKG_DIRNAME)
	rm -rf $(PKG_DIRNAME)

%.el:

.PHONEY: clean
clean:
	rm -f inline-crypt-*.tar
