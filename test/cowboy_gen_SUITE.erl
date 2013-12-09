-module(cowboy_gen_SUITE).

-export([all/0]).
-export([rest_hello_world/1]).

-include_lib("common_test/include/ct.hrl").

all() ->
    [rest_hello_world].

rest_hello_world(_Config) ->
    Req = cowboy_gen:req(),
    {ok, {200, _, _}} = cowboy_gen:call(Req, toppage_handler).
