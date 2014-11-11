%%%-------------------------------------------------------------------
%%% @author zhongwencool@gmail.com
%%% @copyright (C) 2014, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 11. Nov 2014 5:55 PM
%%%-------------------------------------------------------------------
-module(autoload).

-behaviour(gen_server).

%% API
-export([start_link/0,start/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3,
         inotify_event/3
]).

-define(SERVER, ?MODULE).

-record(state, {ref}).

start() ->
    application:start(autoload).

start_link() ->
    Path = autoload_app:autoload_path(),
    gen_server:start_link({local, ?SERVER}, ?MODULE, Path, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init(Path) ->
    ok = filelib:ensure_dir(Path),
    Ref = inotify:watch(Path),
    inotify:print_events(Ref),
    beam_change(Ref),
    {ok, #state{ref = Ref}}.


handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

beam_change(Ref) ->
    inotify_evt:add_handler(Ref, ?MODULE, []).

inotify_event([], Ref, {Masks, Cookie, OptionalName}) ->
    io:format("[BeamChange] - ~p - ~p ~p ~p~n", [Ref, Masks, Cookie, OptionalName]).

