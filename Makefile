USER = aialferov
PROJECT = ghc-bome
VERSION = latest

PORT = 8080

REBAR = ./rebar3

PREFIX = usr/local

BIN_DIR = bin
BIN_PATH = $(DEST_DIR)/$(PREFIX)/$(BIN_DIR)
BIN_PATH_IN = $(shell $(REBAR) path --bin)

BUILD_DIR = _build
BUILD_DIR_IMAGE = $(BUILD_DIR)/image

all:
	$(REBAR) compile
	$(REBAR) unlock

shell:
	$(REBAR) shell
	$(REBAR) unlock

run: all
	$(BIN_PATH_IN)/$(PROJECT)

install:
	mkdir -p $(BIN_PATH)
	install -p $(BIN_PATH_IN)/$(PROJECT) $(BIN_PATH)

uninstall:
	rm -f $(BIN_PATH)/$(PROJECT)
	rmdir -p $(BIN_PATH) 2> /dev/null || true

clean:
	$(REBAR) clean -a
	$(REBAR) unlock

distclean: clean
	rm -rf $(BUILD_DIR)

docker-build: all
	$(MAKE) install DEST_DIR=$(BUILD_DIR_IMAGE) PREFIX=
	install -p -m 644 Dockerfile $(BUILD_DIR_IMAGE)
	docker build $(BUILD_DIR_IMAGE) -t $(USER)/$(PROJECT):$(VERSION)

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

docker-clean:
	docker images -qf dangling=true | xargs docker rmi
