-module(cb_assets_custom_tags).
-export([img_tag/2,
		 javascript_include_set_tag/2,
         stylesheet_include_set_tag/2
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
    cb_assets_helper:img_tag(Variables, Options).

%% {% javascript_include_set_tag set="application.js" %}
javascript_include_set_tag(Variables, Options) ->
	cb_assets_helper:javascript_include_set_tag(Variables, Options).

%% {% stylesheet_include_set_tag set="application.css" %}
stylesheet_include_set_tag(Variables, Options) ->
    cb_assets_helper:stylesheet_include_set_tag(Variables, Options).