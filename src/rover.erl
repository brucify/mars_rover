%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(rover).

-behaviour(gen_server).

-export([ start_link/1
        , assume_control/1
        , new/2
        , north/1
        , south/1
        , east/1
        , west/1
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(SERVER, ?MODULE).
-define(X_MAX, 10).
-define(Y_MAX, 10).


-record(rover_state, { owner = undefined
                     , location = {0, 0}
                     }).

%%%===================================================================
%%% API
%%%===================================================================

new(Name, Location) ->
  supervisor:start_child(mars_rover_sup, #{ id => Name
                                          , start => {rover, start_link, [Location]}
                                          }).

start_link(Location={X, Y}) when X =< ?X_MAX
                         andalso Y =< ?Y_MAX
                         andalso 0 =< X
                         andalso 0 =< Y ->
  {ok, Pid} = gen_server:start_link(?MODULE, [Location], []),
  case grid:try_start(Pid, Location) of
    ok ->
      {ok, Pid};
    {error, Reason} ->
      gen_server:stop(Pid),
      {error, Reason}
  end.

assume_control(Pid) ->
  gen_server:call(Pid, assume_control).

north(Pid) ->
  gen_server:call(Pid, {move, north}).

south(Pid) ->
  gen_server:call(Pid, {move, south}).

east(Pid) ->
  gen_server:call(Pid, {move, east}).

west(Pid) ->
  gen_server:call(Pid, {move, west}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([Location]) ->
  {ok, #rover_state{location = Location}}.

handle_call(assume_control, {From, _Tag}, State) ->
  {reply, ok, State#rover_state{owner = From}};
handle_call({move, Direction}, {Owner, _Tag}, State=#rover_state{ location = Location0
                                                                , owner = Owner
                                                                }) ->
  Location1 = try_move(Direction, Location0),
  print_move(Location1, Location0),
  {reply, {ok, Location1}, State#rover_state{location = Location1}};
handle_call({move, _}, _From, State) ->
  {reply, {error, forbidden}, State}.

handle_cast(_Request, State = #rover_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #rover_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #rover_state{}) ->
  ok.

code_change(_OldVsn, State = #rover_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_move(Direction, L0) ->
  L1 = new_location(Direction, L0),
  case grid:try_move(self(), L1, L0) of
    {ok, L} -> L;
    _       -> L0
  end.

new_location(north, {X, Y}) when X+1 =< ?X_MAX  -> {X+1, Y};
new_location(south, {X, Y}) when 0 =< X-1       -> {X-1, Y};
new_location(east,  {X, Y}) when Y+1 =< ?Y_MAX  -> {X, Y+1};
new_location(west,  {X, Y}) when 0 =< Y-1       -> {X, Y-1};
new_location(_,     {X, Y})                     -> {X, Y}.

print_move(Location, Location) -> print_unable(Location);
print_move(Location,        _) -> print_moved(Location).

print_moved({X, Y}) -> io:format("Rover moved successfully to ~p, ~p~n", [X, Y]).

print_unable({X, Y}) -> io:format("Rover unable to move. Still at ~p, ~p~n", [X, Y]).