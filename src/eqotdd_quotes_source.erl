-module(eqotdd_quotes_source).

-callback last_changed() -> term().
-callback length() -> non_neg_integer().
-callback get(non_neg_integer()) -> {'ok', [byte()]} | {'end', Length :: non_neg_integer()}.
