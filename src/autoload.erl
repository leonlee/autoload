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

-define(LOG(Format,Args),file:write_file(autoload_app:autoload_log(),[lists:flatten(io_lib:format(Format,Args))],[append])).

%%-define(LOG(Format,Args),io:format(Format,Args)).

-record(state, {ref,file_handle}).

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
    backup(),
    Ref = inotify:watch(Path),
    %%inotify:print_events(Ref),
    ?LOG("Path:~p~n",[Path]),
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

inotify_event([], _Ref,{inotify_msg, [modify], _Cookie, FileName}) ->
    ?LOG("modify:~p~n",[FileName]),
    do_modify_file(FileName);

inotify_event([], _Ref,{inotify_msg, [delete], _Cookie, FileName}) ->
    ?LOG("delete:~p~n",[FileName]),
    do_delete_file(FileName);
inotify_event([], _Ref,{inotify_msg, _Masks, _Cookie, _OptionalName}) ->
    %%?LOG("unknow:~p~n",[_Masks]),
    ok.

connect_to_nodes() ->
    NodeCookies = autoload_app:autoload_node_cookies(),
    Nodes = [begin Node end||{Node,_Cookie} <- NodeCookies],
    NotConnects = Nodes -- nodes(),
    lists:foldl(fun(Node,NoConnect) ->
                        {_,Cookie} = lists:keyfind(Node,1,NodeCookies),
                        erlang:set_cookie(Node,Cookie),
                        case net_kernel:connect_node(Node) of
                            true -> NoConnect;
                            false -> [Node|NoConnect];
                            ignored ->[Node|NoConnect]
                        end
                end,[],NotConnects).

do_modify_file(FileName) ->
    case file_to_beam(FileName) of
        {ok, Beam } ->
            NoConnects = connect_to_nodes(),
            c:nl(Beam),
            ?LOG("File:~p update on:~p~nFile:~p not update on ~p~n",[FileName,nodes(),FileName,NoConnects]);
        ignore  ->
            ok
    end.

do_delete_file(FileName) ->
    case file_to_beam(FileName) of
        {ok, Beam } ->
            NoConnects = connect_to_nodes(),
            rpc:multicall(c,l,[Beam]),
            ?LOG("File:~p delete on:~p~nFile:~p not deltete on:~p~n",[FileName,nodes(),FileName,NoConnects]);
        ignore -> 
            ?LOG("Ignore delete File:~p~n",[FileName])
    end.


file_to_beam(FileName) ->
    case filename:extension(FileName) =:= ".beam" of
        true ->
            {ok,list_to_atom(lists:reverse(lists:reverse(FileName) --"maeb.")) };
        false ->
            ignore
    end.

backup() ->
    FileName = autoload_app:autoload_log(),
    case file:read_file_info(FileName) of
        {ok, _FileInfo} ->
            ok = file:rename(FileName, FileName ++ ".bak"),
            ok;
        {error, _Reason} ->
            ok
    end.
