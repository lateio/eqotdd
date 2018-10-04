-module(eqotdd_udp_sup).

-behavior(supervisor).
-export([init/1]).

-export([
    start_link/0
]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).


init(_) ->
    {ok, { { simple_one_for_one, 10, 5}, [
        #{
            id => nil,
            start => {eqotdd_udp, start_link, []},
            restart => permanent,
            shutdown => 2000,
            type => worker
        }
    ]} }.
