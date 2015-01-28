-module(cowboy_gen).

-export([req/0,
         req/1]).
-export([call/2,
         call/3]).
-export([name/0,
         send/2]).

-type response() :: {integer(), proplists:proplist(), binary()}.
-export_types([reponse/0]).

-type version() :: {pos_integer(), pos_integer(), pos_integer()}.
-type version_error() :: no_cowboy_app_in_path | unsupported_cowboy_version.

-define(DEFAULT_TIMEOUT, 5000).

-spec req() -> {ok, cowboy_req:req()} | {error, version_error()}.
req() ->
    req([]).

-spec req(proplists:proplist()) -> {ok, cowboy_req:req()} |
                                   {error, version_error()}.
req(Props) ->
    case get_cowboy_version() of
        {ok, CowboyVersion} ->
            {ok, req(Props, CowboyVersion)};
        {error, _} = Error ->
            Error
    end.

-spec req(proplists:proplist(), string()) -> cowboy_req:req().
req(Props, CowboyVersion) ->
    Socket = undefined,
    Transport = cowboy_gen,
    Peer = proplists:get_value(peer, Props, default_peer(CowboyVersion)),
    Method = proplists:get_value(method, Props, <<"GET">>),
    Path = <<>>,
    Query = <<>>,
    Version = set_version(CowboyVersion),
    Headers = proplists:get_value(headers, Props, []),
    Headers2 = set_default_headers(Headers),
    Host = <<"127.0.0.1">>,
    Port = undefined,
    Buffer = proplists:get_value(body, Props, <<>>),
    Headers3 = set_body_length(Buffer, Headers2),
    CanKeepalive = true,
    Compress = false,
    OnResponse = undefined,
    Req = new(CowboyVersion, Socket, Transport, Peer, Method,
              Path, Query, Version, Headers3, Host, Port,
              Buffer, CanKeepalive, Compress, OnResponse),
    QsVals = proplists:get_value(qs_vals, Props, []),
    cowboy_req:set([{qs_vals, QsVals}], Req).

-spec default_peer(version()) ->
    undefined | {inet:ip_address(), inet:port_number()}.
default_peer(Version) when Version >= {0, 8, 5} ->
    {{0,0,0,0}, 0};
default_peer(_Version) ->
    undefined.

-spec set_version(version()) -> 'HTTP/1.1' | {1, 1}.
set_version(Version) ->
    case Version >= {0, 8, 5} of
        true ->
            'HTTP/1.1';
        false ->
            {1, 1}
    end.

-spec set_default_headers(proplists:proplist()) -> proplists:proplist().
set_default_headers(Headers) ->
    maybe_set({<<"accept">>, <<"*/*">>}, Headers).

-spec set_body_length(binary(), cowboy:http_headers()) -> cowboy:http_headers().
set_body_length(Body, Headers) ->
    Length = list_to_binary(integer_to_list(byte_size(Body))),
    set({<<"content-length">>, Length}, Headers).

new(CowboyVersion, Socket, Transport, Peer, Method, Path, Query, Version,
    Headers, Host, Port, Buffer, CanKeepalive, Compress, OnResponse) ->
    case CowboyVersion >= {0, 8, 5} of
        true ->
            cowboy_req:new(Socket, Transport, Peer, Method, Path, Query,
                           Version, Headers, Host, Port, Buffer,
                           CanKeepalive, Compress, OnResponse);
        false ->
            Fragment = undefined,
            cowboy_req:new(Socket, Transport, Peer, Method, Path, Query,
                           Fragment, Version, Headers, Host, Port, Buffer,
                           CanKeepalive, Compress, OnResponse)
    end.

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

-spec get_cowboy_version() -> {ok, version()} | {error, version_error()}.
get_cowboy_version() ->
    case application:load(cowboy) of
        ok ->
            Res = get_version(),
            application:unload(cowboy),
            Res;
        {error, {already_loaded, cowboy}} ->
            get_version();
        {error, _} ->
            {error, no_cowboy_app_in_path}
    end.

-spec get_version() -> {ok, version()} | {error, version_error()}.
get_version() ->
    {ok, VersionStr} = application:get_key(cowboy, vsn),
    Version = parse_version(VersionStr),
    case Version < {0, 8, 0} of
        true ->
            {error, unsupported_cowboy_version};
        false ->
            {ok, Version}
    end.

-spec parse_version(string()) -> version().
parse_version(VersionStr) ->
    VersionList = re:split(VersionStr, "\\."),
    Version = [list_to_integer(binary_to_list(X)) || X <- VersionList],
    list_to_tuple(Version).
