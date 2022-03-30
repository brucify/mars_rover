%%%-------------------------------------------------------------------
%% @doc mars_rover top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(mars_rover_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
-spec init(Args :: term()) -> {ok, Result} | ignore | {error, Reason :: term()}
  when Result :: { SupFlags :: { RestartStrategy :: supervisor:strategy()
                               , MaxR            :: non_neg_integer()
                               , MaxT            :: non_neg_integer()
                               }
                 , [ChildSpec :: supervisor:child_spec()]
                 }.
init([]) ->
  SupFlags = #{ strategy => one_for_all
              , intensity => 0
              , period => 1
              },
  ChildSpecs = [ #{ id    => grid
                  , start => {grid, start_link, []}
                  , restart => transient
                  }
               ],
  {ok, {SupFlags, ChildSpecs}}.

%% internal functions
