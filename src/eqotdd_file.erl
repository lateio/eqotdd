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

-module(eqotdd_file).

-behavior(eqotdd_quotes_source).
-export([
    get/1,
    length/0,
    last_changed/0
]).

-type eof_return() :: {'end', Length :: non_neg_integer()}.
-type get_return() :: {'ok', [byte()]} | eof_return().
-type byte_iolist() :: list(byte_iolist() | byte()).
-type byte_string() :: list(byte()).
-type get_acc() :: list(byte() | list(byte()) | 'empty').

-spec get(non_neg_integer()) -> get_return().
get(Index) ->
    get(Index, quotes_file_path()).


-spec length() -> non_neg_integer().
length() ->
    {'end', Length} = get(-1, quotes_file_path()),
    Length.


-spec last_changed() -> term().
last_changed() ->
    filelib:last_modified(quotes_file_path()).


-spec quotes_file_path() -> file:filename().
quotes_file_path() ->
    {ok, Filespec} = application:get_env(quotes_file),
    case Filespec of
        {priv_file, App, File}  -> filename:join(code:priv_dir(App), File);
        {file, Path} -> Path
    end.


-spec get
    (non_neg_integer(), file:filename()) -> get_return();
    (-1, file:filename()) -> eof_return().
get(Index, Path) ->
    {ok, Fd} = file:open(Path, [read,{encoding, latin1}]),
    get_line(Fd, [], Index, 0).


-spec get_line
    (file:io_device(), get_acc(), non_neg_integer(), non_neg_integer()) -> get_return();
    (file:io_device(), get_acc(), -1, non_neg_integer()) -> eof_return().
get_line(Fd, Acc, Index, Current) ->
    case file:read_line(Fd) of
        {ok, [$%|_]} -> get_line(Fd, Acc, Index, Current);
        {ok, [$ |_] = Line} -> continue_quote(Line, Fd, Acc, Index, Current);
        {ok, [$\t|_] = Line} -> continue_quote(Line, Fd, Acc, Index, Current);
        {ok, Line} -> new_quote(prune_line(Line), Fd, Acc, Index, Current);
        eof ->
            ok = file:close(Fd),
            case Current of
                Index -> return_line(Acc);
                _ ->
                    Length = case Acc of
                        [] -> Current;
                        _ -> Current + 1
                    end,
                    {'end', Length}
            end
    end.


-spec new_quote
    (byte_string(), file:io_device(), get_acc(), non_neg_integer(), non_neg_integer()) -> get_return();
    (byte_string(), file:io_device(), get_acc(), -1, non_neg_integer()) -> eof_return().
new_quote([], Fd, [], Index, Current) ->
    % Empty line outside of a quote
    get_line(Fd, [], Index, Current);
new_quote([], Fd, Acc, Index, Current) ->
    % Empty line in a quote
    get_line(Fd, [empty|Acc], Index, Current);
new_quote(Line, Fd, [], Index, Current) ->
    % New quote start
    get_line(Fd, [Line], Index, Current);
new_quote(Line, Fd, Acc, Index, Current) ->
    case Current of
        Index ->
            ok = file:close(Fd),
            return_line(Acc);
        _ -> get_line(Fd, [Line], Index, Current+1)
    end.


-spec continue_quote
    (byte_string(), file:io_device(), get_acc(), non_neg_integer(), non_neg_integer()) -> get_return();
    (byte_string(), file:io_device(), get_acc(), -1, non_neg_integer()) -> byte_string() | eof_return().
continue_quote(_, Fd, [], Index, Current) ->
    get_line(Fd, [], Index, Current);
continue_quote(_, Fd, Acc, Index, Current) when Index =/= Current ->
    get_line(Fd, Acc, Index, Current);
continue_quote(Line, Fd, Acc0, Index, Current) ->
    case prune_line(Line) of
        [] -> get_line(Fd, Acc0, Index, Current);
        Pruned ->
            Acc1 = case Acc0 of
                [empty|_] -> [Pruned|Acc0];
                _ -> [Pruned, $  |Acc0]
            end,
            get_line(Fd, Acc1, Index, Current)
    end.


-spec prune_line(byte_string()) -> byte_string().
prune_line(Line) ->
    prune_line(Line, start).


-spec prune_line(byte_string(), 'start' | 'end') -> byte_string().
prune_line([$\t|Rest], Dir) ->
    prune_line(Rest, Dir);
prune_line([$ |Rest], Dir) ->
    prune_line(Rest, Dir);
prune_line(Line, start) ->
    case lists:reverse(Line) of
        [$\n|Rest] -> prune_line(Rest, 'end');
        Rest -> prune_line(Rest, 'end')
    end;
prune_line(Line, 'end') ->
    case lists:reverse(Line) of
        [$%|_] -> []; % Comment
        [$\\|[$%|_] = Rest] -> Rest; % Escaped percent sign
        Reversed -> Reversed
    end.


-spec return_line(get_acc()) -> {'ok', byte_string()}.
return_line(Parts) ->
    {ok, LB} = application:get_env(line_break),
    case application:get_env(append_line_break) of
        {ok, true} -> return_line(Parts, [LB], LB);
        _ -> return_line(Parts, [], LB)
    end.


-spec return_line(get_acc(), byte_iolist(), list(byte()) | byte()) -> {'ok', byte_string()}.
return_line([], Acc, _) ->
    {ok, lists:flatten(Acc)};
return_line([empty|Rest], [], LB) ->
    return_line(Rest, [], LB);
return_line([empty|Rest], [LB|_] = Acc, LB) ->
    return_line(Rest, Acc, LB);
return_line([empty|Rest], Acc, LB) ->
    return_line(Rest, [LB|Acc], LB);
return_line([Head|Rest], Acc, LB) ->
    return_line(Rest, [Head|Acc], LB).
