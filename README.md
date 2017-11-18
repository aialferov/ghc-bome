# GHC Bome Service

The service provides HTTP REST API for storing and accessing body measurement
characteristics. The implementation is based on
[GHC Bome REST](http://github.com/aialferov/ghc_bome_rest) and
[GHC Bome Database](http://github.com/aialferov/ghc_bome_db) Erlang
applications. Please refer to these applications for more implementation
details.

## Usage

You can use Docker to run the service straight away without any other dependency
needed:

```
$ docker run --rm -it -p 8080:8080 aialferov/ghc-bome
```

If this not a first time when you run the service this way, you might need to
update it:

```
$ docker pull aialferov/ghc-bome
```

### Service console

After you ran the service you get into the service console. There you can
run commands to get some information about the service, for example about
API it provides or the API usage example.

### API

Please refer the "API" section of
[GHC Bome REST](http://github.com/aialferov/ghc_bome_rest) documentation.

### API usage example

Create:
```
$ curl -XPUT localhost:8080/v1/users/john \
> -d'{"weight":"60kg","height":"170cm","eyes":"blue"}'
```

Overwrite:
```
$ curl -XPUT localhost:8080/v1/users/john \
> -d'{"weight":"70kg","height":"170cm","eyes":"blue"}'
```

Update:
```
$ curl -XPATCH localhost:8080/v1/users/john -d'{"height":"180cm"}'
```

Read:
```
$ curl localhost:8080/v1/users/john
{"eyes":"blue","height":"180cm","weight":"70kg"}

$ curl localhost:8080/v1/users/john?filter=height,weight
{"height":"180cm","weight":"70kg"}
```

Delete:
```
$ curl -XDELETE localhost:8080/v1/users/john \
> -d'["height","weight"]'
```

## Sources

To build the service from sources you will need "git", "make" and
[Erlang](https://www.erlang-solutions.com/resources/download.html).

Download the sources and build the service executable:

```
$ git clone git://github.com/aialferov/ghc-bome && cd ghc-bome
$ make
```

Once the executable is built it is located in "_build/default/bin" and could
be run immediately:

```
$ _build/default/bin/ghc-bome
```

Also it could be shipped into any machine with Erlang installed and run there.

There are more "make" targets provided for convenience:

```
$ make run       # build and run built service
$ make install   # install service into the system
$ make uninstall # uninstall service
```

### Docker image

Makefile also provides targets to build, push and run service based docker
image. 

```
$ make docker-build # build image
$ make docker-push  # push image (you need to be logged in, see "docker login")
$ make docker-run   # run container with console attached
$ make docker-start # run container in background
$ make docker-stop  # stop container
$ make docker-clean # remove dangling (<none>:<none>) images
```

## Tests

There are bunch of tests provided for testing the API. Please refer
[GHC Bome Acceptance Tests](http://github.com/aialferov/ghc-bome-ac) for more
details.
