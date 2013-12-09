-module(cowboy_gen).

-export([req/0,
         req/1]).
-export([call/2,
         call/3]).
-export([name/0,
         send/2]).

-type response() :: {integer(), proplists:proplist(), binary()}.

-define(DEFAULT_TIMEOUT, 5000).

-spec req() -> cowboy_req:req().
req() ->
    req([]).

-spec req(proplists:proplist()) -> cowboy_req:req().
req(Props) ->
    Socket = undefined,
    Transport = cowboy_gen,
    Peer = undefined,
    Method = proplists:get_value(method, Props, <<"GET">>),
    Path = <<>>,
    Query = <<>>,
    Version = 'HTTP/1.1',
    Headers = proplists:get_value(headers, Props, []),
    Headers2 = set_default_headers(Headers),
    Host = <<"127.0.0.1">>,
    Port = undefined,
    Buffer = proplists:get_value(body, Props, <<>>),
    Headers3 = set_body_length(Buffer, Headers2),
    CanKeepalive = true,
    Compress = false,
    OnResponse = undefined,
    Req = cowboy_req:new(Socket, Transport, Peer, Method, Path, Query,
                         Version, Headers3, Host, Port, Buffer,
                         CanKeepalive, Compress, OnResponse),
    QsVals = proplists:get_value(qs_vals, Props, []),
    cowboy_req:set([{qs_vals, QsVals}], Req).

-spec set_default_headers(proplists:proplist()) -> proplists:proplist().
set_default_headers(Headers) ->
    maybe_set({<<"accept">>, <<"*/*">>}, Headers).

-spec set_body_length(binary(), cowboy:http_headers()) -> cowboy:http_headers().
set_body_length(Body, Headers) ->
    Length = list_to_binary(integer_to_list(byte_size(Body))),
    set({<<"content-length">>, Length}, Headers).

-spec call(cowboy_req:req(), module()) -> {ok, response()} |
                                          {error, timeout}.
call(Req, Handler) ->
    call(Req, Handler, ?DEFAULT_TIMEOUT).

-spec call(cowboy_req:req(), module(), integer()) -> {ok, response()} |
                                                     {error, timeout}.
call(Req, Handler, Timeout) ->
    setup_cowboy_clock(),
    Env = [{handler, Handler},
           {handler_opts, []}],
    cowboy_handler:execute(Req, Env),
    Res = wait_for_response(Timeout),
    teardown_cowboy_clock(),
    Res.

-spec setup_cowboy_clock() -> any().
setup_cowboy_clock() ->
    case ets:info(cowboy_clock) of
        undefined ->
            ets:new(cowboy_clock, [named_table]),
            Time = cowboy_clock:rfc1123(calendar:local_time()),
            ets:insert(cowboy_clock, {rfc1123, Time});
        _Else ->
            ok
    end.

-spec teardown_cowboy_clock() -> any().
teardown_cowboy_clock() ->
    Self = self(),
    case ets:info(cowboy_clock, owner) of
        Self ->
            ets:delete(cowboy_clock);
        _Else ->
            ok
    end.

name() ->
    ?MODULE.

send(_Socket, Response) ->
    [Status, Headers, <<"\r\n">>, Body] = Response,
    Code = status_to_int(Status),
    Headers2 = [{H, V} || [H, _, V, _] <- Headers],
    self() ! {ok, {Code, Headers2, Body}}.

-spec wait_for_response(integer()) -> {ok, response()} | {error, timeout}.
wait_for_response(Timeout) ->
    receive
        {ok, _} = Res ->
            Res
    after
        Timeout ->
            {error, timeout}
    end.

-spec status_to_int(binary()) -> integer().
status_to_int(Status) ->
    Re = <<"^HTTP/1\.\\d (\\d+)">>,
    Opts = [{capture, all_but_first, binary}],
    {match, [CodeBin]} = re:run(Status, Re, Opts),
    list_to_integer(binary_to_list(CodeBin)).

-spec maybe_set({binary(), binary()},
                proplists:proplist()) -> proplists:proplist().
maybe_set({Key, _} = Prop, Proplist) ->
    case lists:keymember(Key, 1, Proplist) of
        true ->
            Proplist;
        false ->
            set(Prop, Proplist)
    end.

-spec set({binary(), binary()}, proplists:proplist()) -> proplists:proplist().
set({Key, _} = Prop, Proplist) ->
    lists:keystore(Key, 1, Proplist, Prop).
