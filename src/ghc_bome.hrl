-define(Help,
    "h|help         Print this message~n"
    "a|api          Print REST API reference~n"
    "e|example      Print REST API usage example~n"
    "q|quit|exit    Shutdown the service and exit console~n"
).
-define(Example,
    "$ curl -XPUT localhost:$port/v1/users/john \~n"
    "> -d'{\"weight\":\"60kg\",\"height\":\"170cm\",\"eyes\":\"blue\"}'~n"
    "~n"
    "$ curl -XPUT localhost:$port/v1/users/john \~n"
    "> -d'{\"weight\":\"70kg\",\"height\":\"170cm\",\"eyes\":\"blue\"}'~n"
    "~n"
    "$ curl -XPATCH localhost:$port/v1/users/john -d'{\"height\":\"180cm\"}'~n"
    "~n"
    "$ curl localhost:$port/v1/users/john~n"
    "{\"eyes\":\"blue\",\"height\":\"180cm\",\"weight\":\"70kg\"}~n"
    "~n"
    "$ curl localhost:$port/v1/users/john?filter=height,weight~n"
    "{\"height\":\"180cm\",\"weight\":\"70kg\"}~n"
    "~n"
    "$ curl -XDELETE localhost:$port/v1/users/john -d'[\"height\",\"weight\"]'~n"
).
-define(Greeting,
    "Welcome to the GHC Bome Service console!~n"
    "~n"
    "Console usage:~n"
    ?Help
).
-define(Farewell, "Bye.~n").

-define(Prompt, "ghc-bome> ").
-define(PromptExit,
    "This will stop the service "
    "and make API unavailable, continue? [y/n]: "
).

-define(Actions, #{
    help => ["h", "help"],
    api => ["a", "api"],
    example => ["e", "example"],
    exit => ["q", "quit", "exit"]
}).
