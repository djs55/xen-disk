
.PHONY: dist/build/vhddisk/vhddisk
dist/build/vhddisk/vhddisk: configure.done
	obuild build

configure.done: vhddisk.obuild
	obuild configure
	touch configure.done

.PHONY: install uninstall clean

install: dist/build/vhddisk/vhddisk
	mkdir -p $(BINDIR)
	cp dist/build/vhddisk/vhddisk $(BINDIR)/vhddisk

uninstall:
	rm -f $(BINDIR)/vhddisk

clean:
	obuild clean
	rm -f configure.done

