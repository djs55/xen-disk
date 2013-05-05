
.PHONY: dist/build/blkback/blkback
dist/build/blkback/blkback: configure.done
	obuild build

configure.done: blkback.obuild
	obuild configure
	touch configure.done

.PHONY: install uninstall clean

install: dist/build/blkback/blkback
	mkdir -p $(BINDIR)
	cp dist/build/blkback/blkback $(BINDIR)/blkback

uninstall:
	rm -f $(BINDIR)/blkback

clean:
	obuild clean
	rm -f configure.done

