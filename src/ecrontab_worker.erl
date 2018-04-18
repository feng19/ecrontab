-module(ecrontab_worker).
-behaviour(gen_server).
-include("ecrontab.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/1,
    add/2,
    del/2,
    start_tasks/1
]).

-record(state, {name, task_count = 0, max_task_count = infinity, now_seconds}).
-record(server_task, {name, spec, mfa, options}).
-define(DICTIONARY_KEY(Seconds), {ecrontab, Seconds}).

%%%===================================================================
%%% API
%%%===================================================================

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

-spec add(pid(), ecrontab:task()) -> ok | {error, atom()}.
add(Pid, Task) ->
    try
        gen_server:call(Pid, {add, Task})
    catch
        {'EXIT', Reason} ->
            {error, Reason}
    end.

-spec del(pid(), ecrontab:task_name()) -> ok | {error, any()}.
del(_Pid, undefined) -> {error, undefined_name};
del(Pid, TaskName) ->
    gen_server:cast(Pid, {del, TaskName}).

-spec start_tasks(pid()) -> ok.
start_tasks(Pid) ->
    gen_server:cast(Pid, start_tasks).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init({WorkerName}) ->
    init(WorkerName, ?ONE_PROCESS_MAX_TASKS_COUNT, true, []);
init({WorkerName, MaxTaskCount}) when is_integer(MaxTaskCount) ->
    init(WorkerName, MaxTaskCount, true, []);
init({WorkerName, MaxTaskCount, Tasks}) when is_integer(MaxTaskCount) ->
    init(WorkerName, MaxTaskCount, true, Tasks);
init({WorkerName, IsAutoStart}) when is_boolean(IsAutoStart) ->
    init(WorkerName, ?ONE_PROCESS_MAX_TASKS_COUNT, IsAutoStart, []);
init({WorkerName, IsAutoStart, Tasks}) when is_boolean(IsAutoStart) ->
    init(WorkerName, ?ONE_PROCESS_MAX_TASKS_COUNT, IsAutoStart, Tasks);
init({WorkerName, MaxTaskCount, IsAutoStart, Tasks}) when is_integer(MaxTaskCount) andalso is_boolean(IsAutoStart) ->
    init(WorkerName, MaxTaskCount, IsAutoStart, Tasks);
init(#{worker_name := WorkerName} = Map) ->
    MaxTaskCount = maps:get(max_task_count, Map, ?ONE_PROCESS_MAX_TASKS_COUNT),
    IsAutoStart = maps:get(is_auto_start, Map, true),
    Tasks = maps:get(tasks, Map, []),
    init({WorkerName, MaxTaskCount, IsAutoStart, Tasks});
init(Args) ->
    {stop, {error_args, Args}}.

init(WorkerName, MaxTaskCount, IsAutoStart, Tasks) ->
    case ets:insert_new(?ETS_WORKER_NAME_INDEX, {WorkerName, self()}) of
        true ->
            process_flag(trap_exit, true),
            gproc:reg(?GROUP_NAME),
            case MaxTaskCount >= length(Tasks) of
                true ->
                    init_tasks(WorkerName, MaxTaskCount, IsAutoStart, Tasks);
                false ->
                    {stop, over_task_count}
            end;
        false ->
            {stop, duplicate_name}
    end.

init_tasks(WorkerName, MaxTaskCount, true, Tasks0) ->
    case ecrontab:parse_tasks(Tasks0) of
        {ok, Tasks} ->
            State = #state{name = WorkerName, now_seconds = ?TIMESTAMP, max_task_count = MaxTaskCount},
            add_tasks(Tasks, true, State);
        Error ->
            {stop, Error}
    end;
init_tasks(WorkerName, MaxTaskCount, false, Tasks0) -> % not auto start
    case ecrontab:parse_tasks(Tasks0) of
        {ok, Tasks} ->
            put(tasks, Tasks),
            {ok, #state{name = WorkerName, now_seconds = ?TIMESTAMP, max_task_count = MaxTaskCount}};
        Error ->
            {stop, Error}
    end.


handle_call({add, _Task}, _From, #state{task_count = TaskCount} = State) when TaskCount >= State#state.max_task_count ->
    {reply, {error, over_task_count}, State};
handle_call({add, Task}, _From, State) ->
    case do_add(State, Task) of
        {ok, NewState} ->
            {reply, ok, NewState};
        Error ->
            {reply, Error, State}
    end;
handle_call(_Msg, _From, State) ->
    {reply, noknow, State}.

handle_cast({del, TaskName}, State) ->
    NewState = do_del(State, TaskName),
    {noreply, NewState};
handle_cast(start_tasks, State) ->
    case erlang:erase(tasks) of
        undefined ->
            {noreply, State};
        Tasks ->
            {ok, NewState} = add_tasks(Tasks, true, State),
            {noreply, NewState}
    end;
handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({ecrontab_tick, Seconds}, State) ->
    NewState = do_tick(State#state{now_seconds = Seconds}),
    {noreply, NewState};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    ets:delete(?ETS_WORKER_NAME_INDEX, State#state.name),
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.

%% ====================================================================
%% internal API
%% ====================================================================

do_tick(State) ->
    case erase(?DICTIONARY_KEY(State#state.now_seconds)) of
        undefined ->
            State;
        [] ->
            State;
        STasks ->
            NowDatetime = ?TIMESTAMP_TO_DATETIME(State#state.now_seconds),
            loop_tasks(NowDatetime, STasks, State)
    end.

loop_tasks(_, [], State) ->
    State;
loop_tasks(NowDatetime, [STask | STasks], State) ->
    do_spawn_task(STask#server_task.mfa),
    case ecrontab_next_time:next_seconds(STask#server_task.spec, NowDatetime) of
        {ok, NextSeconds} ->
            put_in_list(NextSeconds, STask),
            loop_tasks(NowDatetime, STasks, State);
        _ ->
            loop_tasks(NowDatetime, STasks, State#state{task_count = State#state.task_count - 1})
    end.

do_spawn_task({M, F, A}) ->
    erlang:spawn(M, F, A);
do_spawn_task({Node, M, F, A}) ->
    rpc:cast(Node, M, F, A);
do_spawn_task(Fun) ->
    erlang:spawn(Fun).

add_tasks([], _IsSkipError, State) -> {ok, State};
add_tasks([Task | Tasks], IsSkipError, State) ->
    case do_add(State, Task) of
        {ok, NewState} ->
            add_tasks(Tasks, IsSkipError, NewState);
        {error, _Reason} when IsSkipError ->
            add_tasks(Tasks, IsSkipError, State);
        {error, Reason} ->
            {error, {Task, Reason}}
    end.

do_add(State, Task) ->
    NowDatetime = ?TIMESTAMP_TO_DATETIME(State#state.now_seconds),
    case ecrontab_next_time:next_seconds(Task#task.spec, NowDatetime) of
        {ok, NextSeconds} ->
            STask = task_to_server_task(Task),
            put_in_list(NextSeconds, STask),
            {ok, State#state{task_count = State#state.task_count + 1}};
        Error ->
            Error
    end.

task_to_server_task(Task) ->
    #server_task{name = Task#task.name, spec = Task#task.spec, mfa = Task#task.mfa, options = Task#task.options}.

put_in_list(Seconds, STask) ->
    List =
        case get(?DICTIONARY_KEY(Seconds)) of
            undefined ->
                [STask];
            List0 ->
                [STask | List0]
        end,
    put(?DICTIONARY_KEY(Seconds), List).

do_del(State, TaskName) ->
    lists:foldl(
        fun({?DICTIONARY_KEY(Seconds), List}, Acc) ->
            case del_name(TaskName, Seconds, List) of
                false -> Acc;
                ok ->
                    Acc#state{task_count = State#state.task_count - 1}
            end;
            (_, Acc) -> Acc
        end, State, get()).

del_name(Name, Seconds, List) ->
    case lists:keytake(Name, #server_task.name, List) of
        false ->
            false;
        {value, _, NewList} ->
            put(?DICTIONARY_KEY(Seconds), NewList),
            ok
    end.