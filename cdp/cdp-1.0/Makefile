all: cdp

VER = 1.0
DIST = cdp-$(VER)
FILES = main.c Makefile COPYING README cdp.lsm TODO

DEFINES = -DVER=\"$(VER)\"

cdp: main.c Makefile
	gcc $(DEFINES) -O -o cdp main.c
	strip cdp

.PHONY: install
install: cdp
	cp cdp /usr/local/bin/cdp
	@if [ ! -e /usr/local/bin/cdp1 ]; \
		then \
			echo "ln -s cdp /usr/local/bin/cdp1"; \
			ln -s cdp /usr/local/bin/cdp1;\
	fi

.PHONY: dist
dist: $(DIST).tar.gz

$(DIST).tar.gz: $(FILES)
	mkdir $(DIST)
	cp $(FILES) $(DIST)
	-tar czf $(DIST).tar.gz $(DIST)
	rm -rf $(DIST)
	@if [ -f $(DIST).tar.gz ]; \
		then \
			echo "Made $(DIST).tar.gz"; \
		else \
			echo "Failed!"; \
	fi

.PHONY: dist-clean
dist-clean: 
	rm -f $(DIST).tar.gz

.PHONY: debug
debug: main.c Makefile
	gcc $(DEFINES) -g -o cdp.debug main.c
	gdb cdp.debug

.PHONY: clean real-clean
clean: dist-clean
	rm -f cdp.debug
real-clean: clean
	rm -f cdp *~
