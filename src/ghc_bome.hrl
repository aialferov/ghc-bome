-define(Help,
    "h|help         Print this message~n"
    "a|api          Print REST API~n"
    "e|example      Print REST API usage example~n"
    "q|quit|exit    Exit~n"
).
-define(Api,
    "PUT /v1/<user>~n"
    "{\"<type>\":\"<value>\"}~n"
    "~n"
    "GET /v1/<user>~n"
    "GET /v1/<user>/<type>~n"
    "~n"
    "DELETE /v1/<user>~n"
    "DELETE /v1/<user>/<type>~n"
).
-define(Example,
    "curl -XPUT localhost:8080/v1/john -d'{\"height\":\"180cm\"}'~n"
    "curl -XPUT localhost:8080/v1/john -d'{\"weight\":\"70kg\"}'~n"
    "~n"
    "curl localhost:8080/v1/john~n"
    "{\"height\":\"180cm\",\"weight\":\"70kg\"}~n"
    "~n"
    "curl localhost:8080/v1/john/height~n"
    "{\"height\":\"180cm\"}~n"
    "~n"
    "curl -XDELETE localhost:8080/v1/john/height~n"
    "curl -XDELETE localhost:8080/v1/john~n"
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
