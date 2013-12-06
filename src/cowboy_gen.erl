-module(cowboy_gen).

-export([req/1]).
-export([call/2,
         call/3]).
-export([name/0,
         send/2]).

%% Copied from cowboy_req.erl.
-record(http_req, {
          socket,
          transport,
          connection = keepalive,
          pid,
          method,
          version = 'HTTP/1.1',
          peer,
          host,
          host_info,
          port,
          path,
          path_info,
          qs,
          qs_vals,
          bindings,
          headers,
          p_headers = [],
          cookies,
          meta = [],
          body_state = waiting,
          multipart,
          buffer,
          resp_compress = false,
          resp_state = waiting,
          resp_headers = [],
          resp_body = <<>>,
          onresponse
         }).

-type response() :: {integer(), proplists:proplist(), binary()}.

-define(DEFAULT_TIMEOUT, 5000).

-spec req(proplists:proplist()) -> cowboy_req:req().
req(Props) ->
    Method = proplists:get_value(method, Props, <<"GET">>),
    QsVals = proplists:get_value(qs_vals, Props, []),
    Headers = proplists:get_value(headers, Props, []),
    Body = proplists:get_value(body, Props, <<>>),
    Req = #http_req{transport = cowboy_gen,
                    method = Method,
                    qs_vals = QsVals,
                    headers = Headers ++ [{<<"accept">>, <<"*/*">>}]},
    body(Body, Req).

-spec body(binary(), cowboy_req:req()) -> cowboy_req:req().
body(Body, #http_req{headers = Headers} = Req) ->
    Length = integer_to_binary(byte_size(Body)),
    Headers2 = set(<<"content-length">>, Length, Headers),
    Req#http_req{headers = Headers2,
                 buffer = Body}.

-spec call(cowboy_req:req(), module()) -> {ok, response()} |
                                          {error, timeout}.
call(Req, Handler) ->
    call(Req, Handler, ?DEFAULT_TIMEOUT).

-spec call(cowboy_req:req(), module(), integer()) -> {ok, response()} |
                                                     {error, timeout}.
call(Req, Handler, Timeout) ->
    State = setup(),
    Req2 = Req#http_req{pid = self()},
    Env = [{handler, Handler},
           {handler_opts, []}],
    cowboy_handler:execute(Req2, Env),
    Res = wait_for_response(Timeout),
    teardown(State),
    Res.

-spec setup() -> term().
setup() ->
    case ets:info(cowboy_clock) of
        undefined ->
            ets:new(cowboy_clock, [named_table]),
            Time = cowboy_clock:rfc1123(calendar:local_time()),
            ets:insert(cowboy_clock, {rfc1123, Time});
        _Else ->
            ok
    end.

-spec teardown(term()) -> any().
teardown(_State) ->
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
    binary_to_integer(CodeBin).

-spec set(binary(), binary(), proplists:proplist()) -> proplists:proplist().
set(Key, Value, Proplist) ->
    lists:keystore(Key, 1, Proplist, {Key, Value}).
