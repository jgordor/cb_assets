-module(cb_assets_helper).
-export([javascript_include_set_tag/2]).

javascript_include_set_tag(Variables, Options) ->
	SetName = get_var(set, Variables),
	App = get_app(Options),
	case boss_env:get_env(list_to_atom(App), 
						  list_to_atom("cb_assets_conf_javascripts_set_" ++ SetName),
						  undefined) of
		undefined -> "<!-- Please configure the js sets -->";
		[Path, Files] -> 
			case boss_env:boss_env() of
				production ->
					get_js_include(Path ++ "/" ++ SetName);
				_ ->
					Includes = lists:map(fun(F) -> get_js_include(F) end, Files),
					string:join(Includes, "\n")
			end;
		Other ->
			io:format("==> cb_assets - [Error] - processing Set name ~p, bad config value~n~p~n", [Other])
	end.

%% Private

get_var(Var, Variables) ->
	binary_to_list(proplists:get_value(Var, Variables)).

get_app(Options) ->
	proplists:get_value(application, Options).

get_js_include(Src) ->
	"<script src=\"" ++ Src ++ "\"></script>".