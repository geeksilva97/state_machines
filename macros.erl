-module(macros).
-define(INVALID_COOKIE_ERROR, exit({response_error, invalid_header,
    'The set-cookie header is special and must be set using cowboy_req:set_resp_cookie/3,4.'})).

-define(HANDLE_SET_COOKIE(FunctionName), FunctionName() -> hello).

-export([simulate/0, simulate/1, my_fun/0]).

?HANDLE_SET_COOKIE(my_fun).

simulate() ->
  io:format('Just simulating stuff\n').

simulate(true) ->
  ?INVALID_COOKIE_ERROR.
