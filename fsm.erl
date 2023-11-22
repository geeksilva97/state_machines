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
         done/3,
         initial/3,
         error_state/3
        ]).

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
initial(cast, _, Data) ->
  io:format("Could not pass to another state from initial~n~n"),
  { next_state, error_state, Data + 1 }.

reading(cast, { put_package, data }, Data) ->
  {next_state, reading, Data + 1};
reading(cast, { put_package, trailer }, Data) ->
  io:format("finishing~n~n"),
  {next_state, done, Data + 1};
reading(EventType, EventContent, Data) ->
  io:format("Current state: reading -- directing to handle common events~n"),
  handle_event(EventType, EventContent, Data).

done(_, _, Data) ->
  io:format("we are done!~n"),
  {stop, normal, Data}.

error_state(_, _, Data) ->
  {stop, error}.

terminate(Reason, State, Data) ->
  io:format("Terminating - reason=~p; state=~p; data=~p~n~n", [Reason, State, Data]),
  void.

%% Handle events common to all states
handle_event({call,From}, get_state, Data) ->
    io:format("Calling common event ~n", []),
    {keep_state_and_data,
    { reply, From, Data }};

handle_event(cast, _, Data) ->
  io:format("Calling common event from cast~n", []),
  {stop, error};

handle_event(_, _, Data) ->
    io:format("Calling common event: ~p ~n", [Data]),
    keep_state_and_data.
