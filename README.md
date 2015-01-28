# cowboy_gen 0.2.4 [![Build Status][travis_ci_image]][travis_ci]

Helpers for testing handlers by generating fake but valid Cowboy requests.

**Works with Cowboy versions: `0.10.0`, `0.9.0`, `0.8.0`-`0.8.6`.**

## Usage

### Creating request

``` erlang
-type version_error() :: no_cowboy_app_in_path |
                         unsupported_cowboy_version.
-spec cowboy_gen:req(Parameters :: proplists:proplist()) ->
          {ok, Request :: cowboy_req:req()} |
          {error, Reason :: version_error()}.
```

Following parameters can be specified when creating a fake Cowboy request:

 * `{method, Method :: binary()}`
 * `{headers, Headers :: proplists:proplist()}`
 * `{qs_vals, QueryStringValues :: proplists:proplist()}`
 * `{body, RequestBody :: binary()}`
 * `{peer, {Address :: inet:ip_address(), Port :: inet:port_number()}}`

### Calling handler

``` erlang
-type response() :: {Code :: integer(),
                     Headers :: proplists:proplist(),
                     Body :: binary()}.
-spec cowboy_gen:call(Request :: cowboy_req:req(), HandlerModule :: module()) ->
          {ok, Response :: response()} | {error, timeout}.
```

[travis_ci]: https://travis-ci.org/rpt/cowboy_gen
[travis_ci_image]: https://travis-ci.org/rpt/cowboy_gen.png
