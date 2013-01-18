-module(cb_assets_helper).
-export([javascript_include_set_tag/2,
         stylesheet_include_set_tag/2,
         img_tag/2
        ]).

javascript_include_set_tag(Variables, Options) ->
	SetName = get_var(set, Variables),
	App = get_app(Options),
    Ts = get_ts(App),
	case boss_env:get_env(list_to_atom(App), 
						  list_to_atom("cb_assets_conf_javascripts_set_" ++ SetName),
						  undefined) of
		undefined -> "<!-- Please configure the js sets -->";
		[Path, Files] -> 
			case boss_env:boss_env() of
				production ->
					get_js_include(Path ++ "/" ++ SetName, Ts);
				_ ->
					Includes = lists:map(fun(F) -> get_js_include(F, Ts) end, Files),
					string:join(Includes, "\n")
			end;
		Other ->
			io:format("==> cb_assets - [Error] - processing Set name ~p, bad config value~n~p~n", [Other])
	end.

stylesheet_include_set_tag(Variables, Options) ->
    SetName = get_var(set, Variables),
    App = get_app(Options),
    Ts = get_ts(App),
    case boss_env:get_env(list_to_atom(App), 
                          list_to_atom("cb_assets_conf_stylesheets_set_" ++ SetName),
                          undefined) of
        undefined -> "<!-- Please configure the css sets -->";
        [Path, Files] -> 
            case boss_env:boss_env() of
                production ->
                    get_css_include(Path ++ "/" ++ SetName, Ts);
                _ ->
                    Includes = lists:map(fun(F) -> get_css_include(F, Ts) end, Files),
                    string:join(Includes, "\n")
            end;
        Other ->
            io:format("==> cb_assets - [Error] - processing Set name ~p, bad config value~n~p~n", [Other])
    end.

img_tag(Variables, Options) ->
    App = get_app(Options),
    Src = get_var(src, Variables),
    "<img src=\"" ++ Src ++ get_ts(App) ++ "\" />".

%% Private

get_var(Var, Variables) when is_atom(Var) ->
    get_var(atom_to_list(Var), Variables);
get_var(Var, Variables) ->
	binary_to_list(proplists:get_value(Var, Variables)).

get_app(Options) ->
	proplists:get_value(application, Options).

get_ts(App) when is_list(App) ->
    get_ts(list_to_atom(App));
get_ts(App) ->
    case boss_env:get_env(App, cb_assets_timestamp, undefined) of
        undefined -> "";
        CTs -> "?" ++ CTs
    end.

get_js_include(Src, Ts) ->
	"<script src=\"" ++ Src ++ Ts ++ "\"></script>".

get_css_include(Src, Ts) ->
    "<link href=\"" ++ Src ++ Ts ++ "\" rel=\"stylesheet\">".