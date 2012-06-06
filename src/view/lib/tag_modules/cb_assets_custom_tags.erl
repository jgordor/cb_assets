-module(cb_assets_custom_tags).
-export([img_tag/2,
		 javascript_include_set_tag/2
		]).

% put custom tags in here, e.g.
%
% reverse(Variables, Options) ->
%     lists:reverse(binary_to_list(proplists:get_value(string, Variables))).
%
% {% reverse string="hello" %} => "olleh"
%
% Variables are the passed-in vars in your template

%% {% img_tag src="/static/images/img.jpg" %}
img_tag(Variables, Options) ->
	App = proplists:get_value(application, Options),
	Src = binary_to_list(proplists:get_value(src, Variables)),
	Ts = case boss_env:get_env(list_to_atom(App), cb_assets_timestamp, undefined) of
			 undefined -> "";
			 CTs -> "?" ++ CTs
		 end,
	"<img src=\"" ++ Src ++ Ts ++ "\" />".

%% {% javascript_include_set_tag set="application.js" %}
%% uses the boss.config => cb_assets->javascripts->sets[name]
javascript_include_set_tag(Variables, Options) ->
	cb_assets_helper:javascript_include_set_tag(Variables, Options).