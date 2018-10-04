 % this module is recompiled while the app runs to store the current quote
-module(quote).

-export([current/0]).

-spec current() -> binary().
current() -> <<>>.
