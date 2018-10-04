% ------------------------------------------------------------------------------
%
% Copyright (c) 2018, Lauri Moisio <l@arv.io>
%
% The MIT License
%
% Permission is hereby granted, free of charge, to any person obtaining a copy
% of this software and associated documentation files (the "Software"), to deal
% in the Software without restriction, including without limitation the rights
% to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
% copies of the Software, and to permit persons to whom the Software is
% furnished to do so, subject to the following conditions:
%
% The above copyright notice and this permission notice shall be included in
% all copies or substantial portions of the Software.
%
% THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
% IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
% FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
% AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
% LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
% OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
% THE SOFTWARE.
%
% ------------------------------------------------------------------------------

-module(eqotdd_udp).

-export([
    start_link/2,
    init/2,
    loop/1
]).

start_link(Address, Port) ->
    Pid = spawn_link(?MODULE, init, [Address, Port]),
    {ok, Pid}.


init(Address, Port) ->
    process_flag(trap_exit, true),
    Tail = case Address of
        any -> [];
        {_, _, _, _} -> [{ip, Address}];
        {_, _, _, _, _, _, _, _} -> [{ip, Address}, {ipv6_v6only, true}]
    end,
    {ok, Socket} = gen_udp:open(Port, [{active, false}, binary, {reuseaddr, true}|Tail]),
    loop(Socket).


loop(Socket) ->
    receive
        {'EXIT', _, shutdown} ->
            ok = gen_udp:close(Socket),
            exit(shutdown)
    after
        0 ->
            case gen_udp:recv(Socket, 1500, 500) of
                {ok, {Address, Port, _}} -> return_quote(Socket, Address, Port);
                {error, timeout} -> ?MODULE:loop(Socket)
            end
    end.


return_quote(Socket, Address, Port) ->
    ok = gen_udp:send(Socket, Address, Port, quote:current()),
    eqotdd_server:quote_requested(gen_udp),
    ?MODULE:loop(Socket).
