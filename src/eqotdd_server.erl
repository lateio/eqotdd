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

-module(eqotdd_server).

-behavior(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    start_link/0,
    refresh_quote_now/0,
    quote_requested/1
]).


-ifdef(OTP_RELEASE).
    % OTP21 and later
    -define(LOG, logger).
    -define(LOGINFO, info).
-else.
    -define(LOG, error_logger).
    -define(LOGINFO, info_msg).
-endif.


-define(MAX_REFRESH_TRIES, 20).
-define(SECS_PER_DAY, 86400).


-record(state,{
    prev_hash,
    timer,
    quote_index,
    quotes_length,
    last_changed,
    quotes_source,
    quote_requested=0,
    refresh_tries=0
}).

init(_) ->
    process_flag(trap_exit, true),
    {ok, Source} = application:get_env(quotes_source),
    State = init_state(Source),
    {ok, start_udp(setup_quote(State))}.


-spec init_state(Source :: atom()) -> #state{}.
init_state(Source) ->
    {ok, Timer} = timer:send_after(msecs_until_refresh(), refresh_quote),
    #state{quotes_source=Source,quotes_length=Source:length(),last_changed=Source:last_changed(),timer=Timer}.


-spec msecs_until_refresh() -> non_neg_integer().
msecs_until_refresh() ->
    {ok, NewQuoteAt0} = application:get_env(new_quote_at),
    case NewQuoteAt0 of
        {interval, {_,_,_} = NewQuoteAt} ->
            case calendar:time_to_seconds(NewQuoteAt) of
                UntilRefresh when UntilRefresh > 0 -> timer:seconds(UntilRefresh)
            end;
        {H,M,S} when H >= 0, M >= 0, S >= 0 ->
            NewQuoteAt = {H rem 24, M rem 60, S rem 60},
            {_, Time} = calendar:local_time(),
            Seconds = case calendar:time_to_seconds(NewQuoteAt) - calendar:time_to_seconds(Time) of
                0 -> ?SECS_PER_DAY;
                UntilRefresh when UntilRefresh > 0 -> UntilRefresh;
                UntilRefresh when UntilRefresh < 0 -> UntilRefresh + ?SECS_PER_DAY
            end,
            timer:seconds(Seconds)
    end.


code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


terminate(_, #state{timer=Ref}) ->
    timer:cancel(Ref),
    ok.


handle_call(refresh_quote, _, State) ->
    {reply, ok, refresh_quote(State)};
handle_call(quote_requested_count, _, State=#state{quote_requested=Count}) ->
    {reply, Count, State};
handle_call(_, _, State) ->
    {noreply, State}.


handle_cast({quote_requested, _Transport}, State=#state{quote_requested=Count}) ->
    {noreply, State#state{quote_requested=Count+1}};
handle_cast(_, State) ->
    {noreply, State}.


handle_info(refresh_quote, State) ->
    {ok, Timer} = timer:send_after(msecs_until_refresh(), refresh_quote),
    {noreply, refresh_quote(State#state{timer=Timer})};
handle_info(_, State) ->
    {noreply, State}.


-spec start_link() -> {'ok', pid()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


-spec refresh_quote_now() -> 'ok'.
refresh_quote_now() ->
    gen_server:call(?MODULE, refresh_quote).


-spec quote_requested(Transport :: atom()) -> 'ok'.
quote_requested(Transport) ->
    gen_server:cast(?MODULE, {quote_requested, Transport}).

%
%%
%

-spec refresh_quote(#state{}) -> #state{}.
refresh_quote(State=#state{quote_requested=Count}) ->
    ?LOG:?LOGINFO("Quote refresh. Previous quote was requested ~B times", [Count]),
    setup_quote(State#state{refresh_tries=0}).


-spec setup_quote(#state{}) -> #state{}.
setup_quote(#state{quotes_length=0}) ->
    error(empty_quote_source);
setup_quote(#state{refresh_tries=?MAX_REFRESH_TRIES}) ->
    error(too_many_refresh_tries);
setup_quote(State0=#state{quotes_source=Source,last_changed=LastChanged,prev_hash=PrevHash,quotes_length=Length,quote_index=PrevIndex}) ->
    case Source:last_changed() of
        LastChanged ->
            NewIndex = new_quote_index(Length, PrevIndex),
            {ok, Quote} = Source:get(NewIndex),
            case erlang:phash2(Quote) of
                PrevHash when Length > 1 ->
                    State1 = State0#state{quote_index=NewIndex},
                    setup_quote(inc_refresh_tries(State1));
                PrevHash when Length =:= 1 ->
                    ?LOG:?LOGINFO("Keeping the old quote", []),
                    State0#state{prev_hash=PrevHash,quote_index=NewIndex};
                NewHash ->
                    ok = compile_quote(Quote),
                    State1 = State0#state{prev_hash=NewHash,quote_index=NewIndex,quote_requested=0},
                    enforce_max_quote_length(State1)
            end;
        Fresh ->
            State1 = State0#state{last_changed=Fresh,quotes_length=Source:length()},
            setup_quote(inc_refresh_tries(State1))
    end.


-spec enforce_max_quote_length(#state{}) -> #state{}.
enforce_max_quote_length(State) ->
    {ok, MaxLength} = application:get_env(max_quote_length),
    Quote = quote:current(),
    QuoteLength = byte_size(Quote),
    if
        QuoteLength > MaxLength ->
            case State of
                #state{refresh_tries=?MAX_REFRESH_TRIES} -> error(too_many_refresh_tries);
                #state{quotes_length=1} -> error(only_quote_too_long);
                _ -> setup_quote(inc_refresh_tries(State))
            end;
        true ->
            ?LOG:?LOGINFO("New quote: ~s", [Quote]),
            State
    end.


-spec inc_refresh_tries(#state{}) -> #state{}.
inc_refresh_tries(State=#state{refresh_tries=Tries}) ->
    State#state{refresh_tries=Tries+1}.


-spec new_quote_index(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
new_quote_index(1, _) ->
    0;
new_quote_index(Length, Old) ->
    case application:get_env(quote_order) of
        {ok, in_order} ->
            case Old of
                undefined -> 0;
                _ -> (Old + 1) rem Length
            end;
        {ok, random} ->
            case rand:uniform(Length) - 1 of
                Old -> new_quote_index(Length, Old);
                New -> New
            end
    end.


-spec compile_quote(string()) -> 'ok'.
compile_quote(Quote) ->
    QuoteAbs = lists:map(fun (C) -> {bin_element, 0, {integer, 0, C}, default, default} end, Quote),
    Abs = [
        {attribute, 0, module, quote},
        {attribute, 0, export, [{current, 0}]},
        {function, 0, current, 0, [
            {clause, 0, [], [], [{bin, 0, QuoteAbs}]}
        ]}
    ],
    {ok, quote, Bin} = compile:forms(Abs),
    {module, quote} = code:load_binary(quote, "quote_mem.erl", Bin),
    ok.


-spec bind_address(inet:port_number(), function(), list(atom())) -> [pid()].
bind_address(Port, Fun, [Env]) ->
    case application:get_env(Env) of
        undefined -> Fun(any, Port);
        {ok, Addresses = [_|_]} -> Fun(Addresses, Port)
    end;
bind_address(Port, Fun, [Env|Tail]) ->
    case application:get_env(Env) of
        undefined -> bind_address(Port, Fun, Tail);
        {ok, Addresses = [_|_]} -> Fun(Addresses, Port)
    end.


-spec start_udp(#state{}) -> #state{}.
start_udp(State) ->
    case application:get_env(udp) of
        {ok, true} ->
            {ok, Port} = application:get_env(udp_port),
            bind_address(Port, fun bind_udp/2, [udp_address,address]),
            start_tcp(State);
        _ -> start_tcp(State)
    end.


-spec bind_udp('any' | list(inet:ip_address()), inet:port_number()) -> [pid()].
bind_udp(any, Port) ->
    {ok, Pid} = supervisor:start_child(eqotdd_udp_sup, [any, Port]),
    [Pid];
bind_udp(Addresses = [_|_], Port) ->
    lists:map(fun
        (Address) -> {ok, Pid} = supervisor:start_child(eqotdd_udp_sup, [Address, Port]), Pid
    end, Addresses).


-spec start_tcp(#state{}) -> #state{}.
start_tcp(State) ->
    case application:get_env(tcp) of
        {ok, true} ->
            {ok, Port} = application:get_env(tcp_port),
            bind_address(Port, fun bind_tcp/2, [tcp_address,address]),
            start_tls(State);
        _ -> start_tls(State)
    end.


-spec bind_tcp('any' | list(inet:ip_address()), inet:port_number()) -> [pid()].
bind_tcp(any, Port) ->
    {ok, Pid} = ranch:start_listener(tcp_any, ranch_tcp, [{port, Port}], eqotdd_protocol, []),
    true = erlang:link(Pid),
    [Pid];
bind_tcp(Addresses = [_|_], Port) ->
    lists:map(fun
        (Address = {_, _, _, _}) ->
            {ok, Pid} = ranch:start_listener({Address,Port}, ranch_tcp, [{port, Port}, {ip, Address}], eqotdd_protocol, []),
            true = erlang:link(Pid),
            Pid;
        (Address = {_, _, _, _, _, _, _, _}) ->
            {ok, Pid} = ranch:start_listener({Address,Port}, ranch_tcp, [{port, Port}, {ip, Address}, {ipv6_v6only, true}], eqotdd_protocol, []),
            true = erlang:link(Pid),
            Pid
        end,
    Addresses).


-spec start_tls(#state{}) -> #state{}.
start_tls(State) ->
    case application:get_env(tls) of
        {ok, true} ->
            {ok, Port} = application:get_env(tls_port),
            bind_address(Port, fun bind_tls/2, [tls_address,address]),
            State;
        _ -> State
    end.


-spec bind_tls('any' | list(inet:ip_address()), inet:port_number()) -> [pid()].
bind_tls(any, Port) ->
    {ok, Pid} = ranch:start_listener(tls_any, ranch_ssl, [{port, Port}|tls_opts(any)], eqotdd_protocol, []),
    true = erlang:link(Pid),
    [Pid];
bind_tls(Addresses = [_|_], Port) ->
    lists:map(fun
        (Address) ->
            {ok, Pid} = ranch:start_listener({Address,Port}, ranch_ssl, [{port, Port}|tls_opts(Address)], eqotdd_protocol, []),
            true = erlang:link(Pid),
            Pid
        end,
    Addresses).


-spec tls_opts('any' | inet:ip_address()) -> list({atom(), term()}).
tls_opts(Address) ->
    Cert = case application:get_env(tls_certfile) of
        undefined -> nil;
        {ok, CertPath} -> {certfile, CertPath}
    end,
    Key = case application:get_env(tls_keyfile) of
        undefined -> nil;
        {ok, KeyPath} -> {keyfile, KeyPath}
    end,
    Tail = case Address of
        any -> [];
        {_, _, _, _} -> [{ip, Address}];
        {_, _, _, _, _, _, _, _} -> [{ip, Address}, {ipv6_v6only, true}]
    end,
    lists:filter(
        fun
            (nil) -> false;
            (_) -> true
        end,
    [Cert, Key|Tail]).
