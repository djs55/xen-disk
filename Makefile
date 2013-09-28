BINDIR?=/usr/local/bin

.PHONY: dist/build/xen-disk/xen-disk
dist/build/xen-disk/xen-disk: dist/setup
	obuild build

dist/setup: xen-disk.obuild
	obuild configure

.PHONY: install uninstall clean

install:
	mkdir -p $(BINDIR)
	cp dist/build/xen-disk/xen-disk $(BINDIR)/xen-disk

uninstall:
	rm -f $(BINDIR)/xen-disk

clean:
	obuild clean

