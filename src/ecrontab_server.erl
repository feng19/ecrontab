-module(ecrontab_server).
-behaviour(gen_server).
-include("ecrontab.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/0
]).

-record(state, {tid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link(?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    pg2:join(?GROUP_NAME, self()),
    Tid = ecrontab_task_manager:reg_server(self()),
    {ok, #state{tid = Tid}}.

handle_call(_Msg, _From, State) ->
    {reply, noknow, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ecrontab_tick, Seconds}, State) ->
    do_tick(Seconds, State#state.tid),
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ecrontab_task_manager:unreg_server(State#state.tid),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%% ====================================================================
%% internal API
%% ====================================================================

do_tick(NowSeconds, Tid) ->
    case ets:lookup(Tid, NowSeconds) of
        [] ->
            ok;
        [#next_time{tasks = []}] ->
            ok;
        [#next_time{tasks = Tasks}] ->
            loop_tasks(Tasks)
    end.

loop_tasks([]) ->
    ok;
loop_tasks([Task|Tasks]) ->
    do_spawn_task(Task#task.mfa),
    loop_tasks(Tasks).

do_spawn_task({M,F,A}) ->
    erlang:spawn(M, F, A).