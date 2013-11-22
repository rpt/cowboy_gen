# cowboy_gen

Helpers for testing handlers by generating fake but valid Cowboy requests.

## Usage

### Creating request

``` erlang
-spec cowboy_gen:req(Parameters :: proplists:proplist()) ->
          Request :: cowboy_req:req().
```

Following parameters can be specified when creating a fake Cowboy request:

 * `{method, Method :: binary()}`
 * `{headers, Headers :: proplists:proplist()}`
 * `{qs_vals, QueryStringValues :: proplists:proplist()}`

### Calling handler

``` erlang
-type response() :: {Code :: integer(),
                     Headers :: proplists:proplist(),
                     Body :: binary()}.
-spec cowboy_gen:call(Request :: cowboy_req:req(), HandlerModule :: module()) ->
          {ok, Response :: response()} | {error, Reason :: term()}.
```
