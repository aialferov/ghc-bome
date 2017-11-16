USER = aialferov
PROJECT = ghc-bome
PORT = 8080

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

image-build: all
	mkdir _buildimage
	install -p Dockerfile _buildimage
	$(MAKE) install DESTDIR=_buildimage PREFIX=
	docker build _buildimage -t $(USER)/$(PROJECT)
	$(MAKE) uninstall DESTDIR=_buildimage PREFIX=
	rm -f _buildimage/Dockerfile
	rmdir _buildimage

image-push: image-build
	docker push $(USER)/$(PROJECT)

image-run: image-build
	docker run --rm -it -p $(PORT):$(PORT) $(USER)/$(PROJECT)
