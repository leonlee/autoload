-module(autoload_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    ensure_start(inotify),
    autoload:start_link(),
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
    application:get_env(inotify,autoload_path).



