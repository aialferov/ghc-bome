-module(ghc_bome).
-export([main/1]).

-include("ghc_bome.hrl").

main(_) ->
    {ok, Config} = load_config(),
    apply_config(Config),

    application:ensure_all_started(?MODULE),
    console().

console() ->
    io:format(?Greeting),
    console_loop(),
    io:format(?Farewell).

console_loop() ->
    Action = case read_input(?Prompt) of
        eof -> eof;
        Command -> maps:fold(console_action_fun(Command), false, ?Actions)
    end,
    case Action of
        {action, help} -> io:format(?Help), console_loop();
        {action, api} -> io:format(?Api), console_loop();
        {action, example} -> io:format(?Example), console_loop();
        {action, exit} -> console_exit();
        false -> console_loop();
        eof -> ok
    end.

console_action_fun(Command) -> fun
    (ActionCandidate, Commands, false) ->
        IsCommand = lists:member(Command, Commands),
        IsCommand andalso {action, ActionCandidate};
    (_ActionCandidate, _Commands, {action, Action}) -> {action, Action}
end.

console_exit() ->
    case read_input(?PromptExit) of
        "y" -> ok;
        "n" -> console_loop();
        _Other -> console_exit()
    end.

read_input(Prompt) ->
    case io:get_line(Prompt) of
        eof -> eof;
        Data -> string:trim(Data, both, "\n")
    end.

load_config() ->
    ProjectName = filename:basename(escript:script_name()),
    ConfigName = "priv/" ++ ProjectName ++ ".config",

    {ok, Binary} = file:read_file(escript:script_name()),
    [_Header, Zip] = binary:split(Binary, <<"PK">>),
    {ok, Files} = zip:extract(<<"PK", Zip/binary>>, [memory]),
    {ConfigName, ConfigBinary} = lists:keyfind(ConfigName, 1, Files),

    {ok, Tokens, _} = erl_scan:string(binary_to_list(ConfigBinary)),
    erl_parse:parse_term(Tokens).

apply_config(Config) ->
    lists:foreach(fun({App, AppConfig}) ->
        lists:foreach(fun({Par, Val}) ->
            application:set_env(App, Par, Val, [{persistent, true}])
        end, AppConfig)
    end, Config).
