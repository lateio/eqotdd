%%%-------------------------------------------------------------------
%% @doc eqotdd public API
%% @end
%%%-------------------------------------------------------------------

-module(eqotdd_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    eqotdd_sup:start_link().

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================
