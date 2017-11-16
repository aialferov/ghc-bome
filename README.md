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

After you ran the srevice you get into the service console. There you can
run commands to get some information about the service, for example about
API it provides or the API usage example.

The console is also used for log output.

### API

The service provides HTTP REST API for storing, retrieving and deleting user
data.

Store data:
```
PUT /v1/<user> {"type":"value"}
```

Retrieve data:
```
GET /v1/<user>[/<type>]
```

If "type" is not specified the whole user data is retrieved.

Delete data:
```
DELETE /v1/<user>[/<type>]
```

If "type" is not specified the whole user data gets deleted.

### API usage example

Store:
```
$ curl -XPUT localhost:8080/v1/john -d'{"weight":"70kg"}'
$ curl -XPUT localhost:8080/v1/john -d'{"height":"180cm"}'
```

Get:
```
$ curl localhost:8080/v1/john/height
{"height":"180cm"}
$ curl localhost:8080/v1/john
{"height":"180cm","weight":"70kg"}
```

Delete:
```
$ curl -XDELETE localhost:8080/v1/john/height
$ curl localhost:8080/v1/john
{"weight":"70kg"}

$ curl -XDELETE localhost:8080/v1/john
$ curl localhost:8080/v1/john
{}
```

## Sources

To build the service from sources you will need "git", "make" and
[Erlang](https://www.erlang-solutions.com/resources/download.html).

Download the sources and build the service binary:

```
$ git clone git://github.com/aialferov/ghc-bome && cd ghc-bome
$ make
```

Once the binary is built it is located in "_build/default/bin" and could be run
immediately:

```
$ _build/default/bin/ghc-bome
```

Also it could be shipped into an any machine with Erlang installed and run
there.

There are more "make" targets provided for convenience:

```
$ make run       # build and run built service
$ make install   # install service into a system
$ make uninstall # uninstall service
```

### Docker image

Makefile also provides targets to build, push and run service based docker
image. 

```
$ make docker-build # build docker image
$ make docker-push  # to push the image you need to be docker logged in
$ make docker-run   # run docker container with console attached
$ make docker-start # run docker container in background
$ make docker-stop  # stop docker container
```
