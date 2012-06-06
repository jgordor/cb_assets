%%%-------------------------------------------------------------------
%%% @author Jose Luis Gordo Romero <jgordor@gmail.com>
%%% http://www.freemindsystems.com
%%% @doc Chicago Boss Assets Management plugin
%%%  Manage combine/minify/browser cache invalidation for assets
%%%  In the future should work with CDN integration to boost page-load
%%%  and by-pass browser connection limitations
%%% @end
%%%-------------------------------------------------------------------
-module(cb_assets_plugin).

-export([cb_assets/2]).

-define(BOSS_CONFIG_FILE, "boss.config").

%% ====================================================================
%% Public API
%% ====================================================================

%%--------------------------------------------------------------------
%% @doc boss command
%% @spec boss(_Config, _AppFile) -> ok | {error, Reason}
%%       Boss enabled rebar commands, usage:
%%       ./rebar boss c=command
%% @end
%%--------------------------------------------------------------------
cb_assets(RebarConf, AppFile) ->
	ok.
