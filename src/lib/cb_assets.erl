%%%-------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% http://www.freemindsystems.com
%%% @doc Chicago Boss Assets Management plugin
%%%  Manage combine/minify/browser cache invalidation for assets
%%%  In the future should work with CDN integration to boost page-load
%%%  and by-pass browser connection limitations
%%% @end
%%%-------------------------------------------------------------------
-module(cb_assets).
-export([combine_and_minify/1,
		 initialize/0
		]).
-define(YUI_VER, "2.4.7").
-define(BOSS_CONFIG_FILE, "boss.config").
-define(ASSETS_CONFIG_FILE, "priv/cb_assets.config").

combine_and_minify(App) ->
    AppPath = boss_env:get_env(App, path, undefined),
    Conf = assets_config(AppPath),
    JConf = case lists:keyfind(javascripts, 1, Conf) of
        false -> [];
        {javascripts, JC} -> JC
    end,
    CConf = case lists:keyfind(stylesheets, 1, Conf) of
        false -> [];
        {stylesheets, CC} -> CC
    end,
    io:format("==> cb_assets - combine and minify js...~n"),
    combine(JConf),
    io:format("==> cb_assets - combine and minify css...~n"),
    combine(CConf),
	io:format("==> cb_assets - generating new timestamp...~n"),
	gen_timestamp(App),
	io:format("==> cb_assets - done~n").

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
    os:cmd("java -jar " ++ PAssetPath ++ "/priv/yuicompressor/yuicompressor-" ++ ?YUI_VER ++ ".jar " ++ Combined ++ " -o " ++ Combined),
    combine_set(Rest).

initialize() ->
	lists:map(fun(App) ->
					  {AppName, AppConf} = App,
					  Path = proplists:get_value(path, AppConf),
					  case get_timestamp_content(Path ++ "/priv/cb_assets.timestamp") of
						  undefined -> skyp;
						  T -> application:set_env(AppName, cb_assets_timestamp, T)
					  end,
					  case assets_config(Path) of
						  undefined -> skyp;
						  AssetsConf -> 
							  %% General
							  application:set_env(AppName, cb_assets_conf, AssetsConf),
							  %% Js
							  JConf = get_conf_value(javascripts, AssetsConf),
							  application:set_env(AppName, cb_assets_conf_javascripts, JConf),
							  JSets = get_conf_value(sets, JConf),
							  application:set_env(AppName, 
												  cb_assets_conf_javascripts_sets, 
												  JSets),
							  lists:map(fun(Set) ->
												{SName, SPath, SFiles} = Set,
												application:set_env(AppName,
																	list_to_atom("cb_assets_conf_javascripts_set_" ++ SName),
																	[SPath, SFiles])
										end, get_info_from_sets(JSets)),
							  %% Css
							  CConf = get_conf_value(stylesheets, AssetsConf), 
							  application:set_env(AppName, cb_assets_conf_stylesheets, CConf),
							  CSets = get_conf_value(sets, CConf),
							  application:set_env(AppName, 
												  cb_assets_conf_stylesheets_sets, 
												  CSets),
							  lists:map(fun(Set) ->
												{SName, SPath, SFiles} = Set,
												application:set_env(AppName,
																	list_to_atom("cb_assets_conf_stylesheets_set_" ++ SName),
																	[SPath, SFiles])
										end, get_info_from_sets(CSets))
					  end
			  end, boss_config()),
	io:format("==> cb_assets - [OK] - initialization done in ~s mode~n", [atom_to_list(boss_env:boss_env())]).

%% Private

get_info_from_sets(Sets) when Sets =:= undefined ->
	[];
get_info_from_sets(Sets) ->
	[{proplists:get_value(name, S), proplists:get_value(path, S), proplists:get_value(files, S)} || S <- Sets].
	
  
gen_timestamp(App) ->
	Path = Conf = boss_env:get_env(App, path, "."),
	file:write_file(Path ++ "/priv/cb_assets.timestamp", io_lib:fwrite("~s", [integer_to_list(get_timestamp())])).

get_timestamp() ->
	{Mega,Sec,Micro} = erlang:now(),
	(Mega*1000000+Sec)*1000000+Micro.

boss_config() ->
    case file:consult(?BOSS_CONFIG_FILE) of
        {error,enoent} ->
            io:format("FATAL: cb_assets - Config file ~p not found.~n", [?BOSS_CONFIG_FILE]),
            halt(1);
        {ok, [BossConfig]} ->
            BossConfig
    end.

assets_config(Path) ->
    case file:consult(Path ++ "/" ++ ?ASSETS_CONFIG_FILE) of
        {error,enoent} ->
            undefined;
        {ok, [AssetsConfig]} ->
            AssetsConfig
    end.

get_timestamp_content(File) ->
	case file:open(File, [read]) of
		{error, _} -> 
			undefined;
		{ok, Dev} ->
			T = io:get_line(Dev, ""),
			file:close(Dev),
			T
	end.

get_conf_value(Val, Conf) when Conf =:= undefined ->
	undefined;
get_conf_value(Val, Conf) ->
	proplists:get_value(Val, Conf).

%%                 case boss_env:is_developing_app(AppName) of
%%                     true -> boss_load:load_all_modules(AppName, TranslatorSupPid);
%%                     false -> ok
%%                 end,