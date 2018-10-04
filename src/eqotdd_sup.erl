%%%-------------------------------------------------------------------
%% @doc eqotdd top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(eqotdd_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%====================================================================
%% API functions
%%====================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%====================================================================
%% Supervisor callbacks
%%====================================================================

%% Child :: #{id => Id, start => {M, F, A}}
%% Optional keys are restart, shutdown, type, modules.
%% Before OTP 18 tuples must be used to specify a child. e.g.
%% Child :: {Id,StartFunc,Restart,Shutdown,Type,Modules}
init([]) ->
    {ok, { {one_for_all, 5, 5}, [
        #{
            id => eqotdd_udp_sup,
            start => {eqotdd_udp_sup, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => supervisor
        },
        #{
            id => eqotdd_server,
            start => {eqotdd_server, start_link, []},
            restart => permanent,
            shutdown => 1000,
            type => worker
        }
    ]} }.

%%====================================================================
%% Internal functions
%%====================================================================
