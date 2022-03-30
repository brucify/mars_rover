%%%-------------------------------------------------------------------
%%% @author bruce
%%% @copyright (C) 2022, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(grid).

-behaviour(gen_server).

-export([ start_link/0
        , try_move/3
        , try_start/2
        ]).

-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-define(SERVER, ?MODULE).

-record(grid_state, {grid = []}).

%%%===================================================================
%%% API
%%%===================================================================


try_start(Pid, L) ->
  gen_server:call(?SERVER, {try_start, Pid, L}).

try_move(Pid, L1, L0) ->
  gen_server:call(?SERVER, {try_move, Pid, L1, L0}).

start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

init([]) ->
  {ok, #grid_state{}}.

handle_call({try_start, Pid, L}, _From, State = #grid_state{grid = Grid0}) ->
  {Reply, Grid} =
    case try_start(Grid0, Pid, L) of
      {ok, Grid1}     -> {ok,              Grid1};
      {error, Reason} -> {{error, Reason}, Grid0}
    end,
  {reply, Reply, State#grid_state{grid = Grid}};
handle_call({try_move, Pid, L1, L0}, _From, State = #grid_state{grid = Grid0}) ->
  {ok, L2, Grid1} = try_move(Grid0, Pid, L1, L0),
  {reply, {ok, L2}, State#grid_state{grid = Grid1}}.

handle_cast(_Request, State = #grid_state{}) ->
  {noreply, State}.

handle_info(_Info, State = #grid_state{}) ->
  {noreply, State}.

terminate(_Reason, _State = #grid_state{}) ->
  ok.

code_change(_OldVsn, State = #grid_state{}, _Extra) ->
  {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

try_move(Grid0, Pid, L1, L0) ->
  case proplists:get_value(L1, Grid0) of
    undefined ->
      {ok, L1, [{L1, Pid}|proplists:delete(L0, Grid0)]};
    _         ->
      {ok, L0, Grid0}
  end.

try_start(Grid0, Pid, L) ->
  case proplists:get_value(L, Grid0) of
    undefined ->
      {ok, [{L, Pid}|Grid0]};
    _         ->
      {error, occupied}
  end.