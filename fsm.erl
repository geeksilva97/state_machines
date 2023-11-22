-module(fsm).
-behaviour(gen_statem).

-export([
         start/0,
         get_state/0,
         simulate/0,
         error_simulation/0
        ]).
-export([callback_mode/0, init/1, terminate/3]).
-export([
         reading/3,
         initial/3
        ]).

-define(HANDLE_COMMON, ?FUNCTION_NAME(T, C, D) -> handle_common(T, C, ?FUNCTION_NAME, D)).

start() ->
  gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

get_state() ->
  gen_statem:call(?MODULE, get_state).

error_simulation() ->
  gen_statem:cast(?MODULE, {put_package, header}),
  timer:sleep(500),
  gen_statem:cast(?MODULE, {put_package, hello_world}),
  ok.

simulate() ->
  gen_statem:cast(?MODULE, {put_package, header}),
  Result = get_state(),
  io:format("Result: ~p~n~n", [Result]),
  timer:sleep(500),
  gen_statem:cast(?MODULE, {put_package, unexpected_info}),
  timer:sleep(500),
  gen_statem:cast(?MODULE, {put_package, trailer}),
  timer:sleep(500),
  gen_statem:cast(?MODULE, {put_package, wtf}),
  ok.

callback_mode() -> state_functions.

init([]) ->
  % {ok, State, Data}
    {ok, initial, 0}.

% state functions - reference -> https://www.erlang.org/doc/man/gen_statem#Module:StateName-3
% events can be -> cast | { call, From }

initial(cast, { put_package, header }, Data) ->
  { next_state, reading, Data + 1 };
?HANDLE_COMMON.

reading(cast, { put_package, data }, Data) ->
  {next_state, reading, Data + 1};
reading(cast, { put_package, trailer }, Data) ->
  io:format("finishing~n~n"),
  {stop, normal, Data + 1};
?HANDLE_COMMON.

terminate(Reason, State, Data) ->
  io:format("Terminating - reason=~p; state=~p; data=~p~n~n", [Reason, State, Data]),
  void.

handle_common(cast, EventContent, State, Data) ->
  io:format("handle_common for cast -- content=~p; state=~p; data=~p~n~n", [EventContent, State, Data]),
  keep_state_and_data;
handle_common({call, From}, EventContent, State, Data) ->
  io:format("handle_common for call -- content=~p; state=~p; data=~p~n~n", [EventContent, State, Data]),
    {keep_state_and_data,
    { reply, From, Data }};
handle_common(_, _, _, _) ->
  {stop, error}.

%% Handle events common to all states
handle_event({call,From}, get_state, Data) ->
    io:format("Calling common event ~n", []),
    {keep_state_and_data,
    { reply, From, Data }};

handle_event(cast, _, Data) ->
  io:format("Calling common event from cast~n", []),
  {stop, normal};

handle_event(_, _, Data) ->
    io:format("Calling common event: ~p ~n", [Data]),
    keep_state_and_data.
