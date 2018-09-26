-module('ghc-bome_test').

-include_lib("eunit/include/eunit.hrl").

-define(M, 'ghc-bome').

main_test() ->
    ?assertEqual(ok, ?M:main([])).
