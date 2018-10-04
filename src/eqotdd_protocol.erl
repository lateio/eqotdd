-module(eqotdd_protocol).

-behavior(ranch_protocol).
-export([start_link/4]).

-export([init/2]).

start_link(Ref, _Socket, Transport, _Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Transport]),
    {ok, Pid}.


init(Ref, Transport) ->
    {ok, Socket} = ranch:handshake(Ref),
    Transport:send(Socket, quote:current()),
    Transport:close(Socket),
    eqotdd_server:quote_requested(Transport).
