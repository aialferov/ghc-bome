PROJECT = ghc-bome

REBAR = ./rebar3

PREFIX = usr/local

BINDIR = bin
BINPATH = $(DESTDIR)/$(PREFIX)/$(BINDIR)
BINPATHIN := $(shell $(REBAR) path --bin)

BUILDDIR = _build

all:
	$(REBAR) compile
	$(REBAR) unlock

check:
	$(REBAR) eunit

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILDDIR)

shell:
	$(REBAR) shell
	$(REBAR) unlock

run: all
	$(BINPATHIN)/$(PROJECT)

install:
	mkdir -p $(BINPATH)
	install -p $(BINPATHIN)/$(PROJECT) $(BINPATH)

uninstall:
	rm -f $(BINPATH)/$(PROJECT)
	rmdir -p $(BINPATH) 2> /dev/null || true
