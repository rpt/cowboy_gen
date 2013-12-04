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
          method = <<"GET">>,
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
          headers = [],
          p_headers = [],
          cookies,
          meta = [],
          body_state = waiting,
          multipart,
          buffer = <<>>,
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
    Method = proplists:get_value(method, Props),
    QsVals = proplists:get_value(qs_vals, Props),
    Headers = proplists:get_value(headers, Props),
    #http_req{transport = cowboy_gen,
              method = method(Method),
              qs_vals = qs_vals(QsVals),
              headers = headers(Headers)}.

-spec method(undefined | binary()) -> binary().
method(undefined) ->
    <<"GET">>;
method(Method) ->
    Method.

-spec qs_vals(undefined | proplists:proplist()) -> proplists:proplist().
qs_vals(undefined) ->
    [];
qs_vals(QsVals) ->
    QsVals.

-spec headers(undefined | proplists:proplist()) -> proplists:proplist().
headers(undefined) ->
    [{<<"Accept">>, <<"*/*">>}];
headers(Headers) ->
    Headers.

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
    catch ets:new(cowboy_clock, [named_table]),
    Time = cowboy_clock:rfc1123(calendar:local_time()),
    ets:insert(cowboy_clock, {rfc1123, Time}).

-spec teardown(term()) -> any().
teardown(_State) ->
    ets:delete(cowboy_clock).

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
