
build: configure.done
	obuild build

configure.done: vhddisk.obuild
	obuild configure
	touch configure.done

.PHONY: clean
clean:
	obuild clean
	rm -f configure.done

