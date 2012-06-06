-module(cb_assets_01_init).

-export([init/0, stop/1]).

init() ->
	io:format("==> cb_assets - initializing...~n"),
	cb_assets:initialize(),
    {ok, []}.

stop(ListOfWatchIDs) ->
    lists:map(fun boss_news:cancel_watch/1, ListOfWatchIDs).