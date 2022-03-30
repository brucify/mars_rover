%%%-------------------------------------------------------------------
%% @doc mars_rover public API
%% @end
%%%-------------------------------------------------------------------

-module(mars_rover_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    mars_rover_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
