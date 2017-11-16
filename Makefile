USER = aialferov
PROJECT = ghc-bome
VERSION = latest
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

docker-build: all
	mkdir -p _buildimage
	install -p Dockerfile _buildimage
	$(MAKE) install DESTDIR=_buildimage PREFIX=
	docker build _buildimage -t $(USER)/$(PROJECT):$(VERSION)
	$(MAKE) uninstall DESTDIR=_buildimage PREFIX=
	rm -f _buildimage/Dockerfile
	rmdir _buildimage

docker-clean:
	docker images -qf dangling=true | xargs docker rmi

docker-push: docker-build
	docker push $(USER)/$(PROJECT):$(VERSION)

docker-run: docker-build
	docker run --name $(PROJECT) --rm -it -p $(PORT):$(PORT) \
		$(USER)/$(PROJECT):$(VERSION)

docker-start: docker-build
	docker run --name $(PROJECT) --rm -d -p $(PORT):$(PORT) \
		$(USER)/$(PROJECT):$(VERSION)

docker-stop:
	docker stop $(PROJECT) -t0
