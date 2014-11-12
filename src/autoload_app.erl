-module(autoload_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-export([autoload_path/0,autoload_node_cookies/0,autoload_log/0]).
%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    autoload_deps:ensure(),
    ensure_start(inotify),
    autoload_sup:start_link().

stop(_State) ->
    application:stop(inotify),
    ok.

ensure_start(App) ->
    case application:start(App) of
        ok ->
            ok;
        {error, {already_started, App}} ->
            ok
    end.

autoload_path() ->
    {ok,Path} = application:get_env(autoload,autoload_path),
    Path.
autoload_node_cookies() ->
    {ok,NodeCookies} = application:get_env(autoload,node_cookie),
    NodeCookies.

autoload_log() ->
    {ok,Log} = application:get_env(autoload,log),
    Log.
