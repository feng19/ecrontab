-module(ecrontab_server).
-behaviour(gen_server).
-include("ecrontab.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/0, start_link/1, start_link/2,
    add/5, remove/3
]).

-record(state, {
            tasks = gb_trees:empty() :: gb_trees:tree(), %% name -> task % todo ets
            queue = gb_trees:empty() :: gb_trees:tree(), %% time -> name
            tref :: reference()       %% the check cron task timer
        }).
-record(task, {spec, mfa, next, options}). % todo no next

-define(CHECK_TASK_INTERVAL, 1000). % 1s
-define(START_TASK_TIMER, erlang:start_timer(?CHECK_TASK_INTERVAL, self(), check_task)).


start_link() ->
    start_link({local, ?MODULE}, []).
start_link(Name) ->
    start_link(Name, []).

start_link(Name,Args) when is_tuple(Name) ->
    gen_server:start_link(Name, ?MODULE, Args, []);
start_link(Name,Args) when is_atom(Name) ->
    gen_server:start_link({local, Name}, ?MODULE, Args, []).

add(Server, Name, Spec, MFA, Options) ->
    gen_server:call(Server, {add, {Name, Spec, MFA, Options}}).

remove(Server, Name, Options) ->
    gen_server:call(Server, {remove, {Name, Options}}).

%% ====================================================================
%% callback API
%% ====================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    Tref = ?CHECK_TASK_INTERVAL,
    {ok, #state{tref=Tref}}.

handle_call({add, {Name, Spec, MFA, Options}}, _From, State) ->
    case do_add(Name, Spec, MFA, Options, State#state.tasks, State#state.queue) of
        {ok, {Tasks, Queue}} ->
            {reply, ok, State#state{tasks=Tasks,queue=Queue}};
        Reply ->
            {reply, Reply, State}
    end;
handle_call({remove, {Name, Options}}, _From, State) ->
    case do_remove(Name, Options, State#state.tasks, State#state.queue) of
        {ok, {Tasks, Queue}} ->
            {reply, ok, State#state{tasks=Tasks,queue=Queue}};
        Reply ->
            {reply, Reply, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, noknow, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, check_task}, State) ->
    {Tasks, Queue} = do_tick(State#state.tasks, State#state.queue),
    {noreply, State#state{tasks=Tasks,queue=Queue,tref=?START_TASK_TIMER}};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    timer:cancel(State#state.tref),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.
    
%% ====================================================================
%% internal API
%% ====================================================================

do_add(Name, Spec, MFA, Options, Tasks, Queue) ->
    case gb_trees:is_defined(Name, Tasks) of
        true  ->
            {error, task_exists};
        false ->
            NowDatetime = erlang:localtime(),
            NowTimestamp = ecrontab_time_util:datetime_to_timestamp(NowDatetime),
            case ecrontab_next_time:next_time(Spec, NowDatetime, NowTimestamp) of
                {ok, Time} ->
                    Task = #task{spec=Spec, mfa=MFA, next=Time, options=Options},
                    {ok, {gb_trees:insert(Name, Task, Tasks), gb_trees:insert({Time, Name}, Name, Queue)}};
                {error, Rsn} ->
                    {error, Rsn}
            end
    end.

do_remove(Name, _Options, Tasks, Queue) ->
    case gb_trees:lookup(Name, Tasks) of
        {value, #task{next=Time}} ->
            {ok, {gb_trees:delete(Name, Tasks), gb_trees:delete_any({Time, Name}, Queue)}};
        none ->
            {error, no_such_task}
    end.

do_tick(Tasks0, Queue0) ->
    case gb_trees:size(Queue0) of
        0 ->
            {Tasks0, Queue0};
        _ ->
            NowDatetime = erlang:localtime(),
            NowTimestamp = ecrontab_time_util:datetime_to_timestamp(NowDatetime),
            case gb_trees:take_smallest(Queue0) of
                {{Time, Name}, Name, Queue1} when Time =< NowTimestamp ->
                    Task = gb_trees:get(Name, Tasks0),
                    do_spawn_task(Task#task.mfa),
                    {Tasks, Queue} = try_schedule(Name, Task, Tasks0, Queue1, NowDatetime, NowTimestamp),
                    do_tick(Tasks, Queue);
                {{_Time, _Name}, _Name, _Queue} ->
                    {Tasks0, Queue0}
            end
    end.

do_spawn_task({M,F,A}) ->
    erlang:spawn(M, F, A).

try_schedule(Name, Task, Tasks0, Queue0, NowDatetime, NowTimestamp) ->
    case ecrontab_next_time:next_time(Task#task.spec, NowDatetime, NowTimestamp) of
        {ok, Time} ->
            lager:info("~p next start: ~p", [Name, Time]),
            {gb_trees:update(Name, Task#task{next=Time}, Tasks0),
             gb_trees:insert({Time, Name}, Name, Queue0)};
        {error, Reason} ->
            lager:info("~p unable to find next start: ~p", [Name, Reason]),
            {gb_trees:update(Name, Task#task{next=undefined}, Tasks0), Queue0}
    end.

