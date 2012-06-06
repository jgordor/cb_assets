-module(cb_assets).
-export([combine_and_minify/1]).
-define(YUI_VER, "2.4.7").

combine_and_minify(App) ->
    Conf = boss_env:get_env(App, cb_assets, undefined),
    JConf = case lists:keyfind(javascripts, 1, Conf) of
        false -> [];
        {javascripts, JC} -> JC
    end,
    CConf = case lists:keyfind(stylesheets, 1, Conf) of
        false -> [];
        {stylesheets, CC} -> CC
    end,
    io:format("==> cb_assets - combine and minify js~n"),
    combine(JConf),
    io:format("==> cb_assets - combine and minify css~n"),
    combine(CConf).

combine(Conf) ->
    Sets = proplists:get_value(sets, Conf),
    combine_set(Sets).

combine_set(undefined) -> ok;
combine_set([]) -> ok;
combine_set([Set|Rest]) ->
    PAssetPath = boss_env:get_env(cb_assets, path, "../cb_assets"),
    Files = lists:map(fun(F) ->
                              "priv" ++ F
                      end, proplists:get_value(files, Set)),
    Path = proplists:get_value(path, Set),
    Name = proplists:get_value(name, Set),
    %% Ensure the path is created
    filelib:ensure_dir("priv" ++ Path ++ "/foo"),
    Combined = "priv" ++ Path ++ "/" ++ Name,
    os:cmd("if [ -e " ++ Combined ++ " ]; then rm " ++ Combined ++ "; fi"),
    os:cmd("cat " ++ string:join(Files, " ") ++ " > " ++ Combined),
    os:cmd("java -jar " ++ PAssetPath ++ "/priv/yuicompressor/yuicompressor-" ++ ?YUI_VER ++ ".jar " ++ Combined ++ " -o " ++ Combined).