-module(ecrontab_task_manager).
-behaviour(gen_server).
-include("ecrontab.hrl").
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-export([
    start_link/0,
    tick/1,
    add/4,
    remove/2,
    reg_server/1,
    unreg_server/1
]).

-define(SERVER, ?MODULE).
-record(state, {server_ets_name_counter=0,server_count=0,servers=[]}).
-record(server, {pid,tid}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

tick(Seconds) ->
    gen_server:cast(?SERVER, {ecrontab_tick, Seconds}).

add(Name, Spec, MFA, Options) ->
    gen_server:call(?SERVER, {add, #task{name = Name, spec = Spec, mfa = MFA, options = Options}}).

remove(Name, Options) ->
    gen_server:call(?SERVER, {remove, {Name, Options}}).

reg_server(Pid) ->
    gen_server:call(?SERVER, {reg_server, Pid}).

unreg_server(Pid) ->
    gen_server:cast(?SERVER, {unreg_server, Pid}).
%%already_exists


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    process_flag(trap_exit, true),
    ets:new(?ETS_NAME_TASKS,[
        {keypos,#task.name},
        {write_concurrency, true},
        {read_concurrency, true}]),
    {ok, #state{}}.

handle_call({add, Task}, _From, State) ->
    case do_add(Task, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
handle_call({remove, {Name, Options}}, _From, State) ->
    case do_remove(Name, Options, State) of
        {ok, NewState} ->
            {reply, ok, NewState};
        Reply ->
            {reply, Reply, State}
    end;
handle_call({reg_server, Pid}, _From, State) ->
    Servers0 = State#state.servers,
    ServerCount = State#state.server_count+1,
    case lists:keytake(undefined, #server.pid, Servers0) of
        false ->
            Counter = State#state.server_ets_name_counter+1,
            Tid = new_server_ets(Counter),
            Servers = [#server{pid = Pid,tid = Tid}|Servers0],
            {reply, Tid, State#state{server_count = ServerCount,server_ets_name_counter = Counter,servers = Servers}};
        {value, Server, Servers1} ->
            Tid = Server#server.tid,
            Servers = [Server#server{pid = Pid}|Servers1],
            {reply, Tid, State#state{server_count = ServerCount,servers = Servers}}
    end;
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({unreg_server, Tid}, State) ->
    Servers0 = State#state.servers,
    case lists:keytake(Tid, #server.tid, Servers0) of
        false ->
            {noreply, State};
        {value, Server, Servers2} ->
            ServerCount = State#state.server_count-1,
            Servers3 = [Server#server{pid = undefined}|Servers2],
            {noreply, State#state{server_count = ServerCount,servers = Servers3}}
    end;
handle_cast({ecrontab_tick, _Seconds}, State) ->
    %todo clean old
    {noreply, State};
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

do_add(Task, State) ->
    case ets:lookup(?ETS_NAME_TASKS,Task#task.name) of
        []  ->
            NowDatetime = erlang:localtime(),
            Spec = Task#task.spec,
            case ecrontab_next_time:next_time(Spec, NowDatetime) of
                {ok, Datetime} ->
                    LastDatetime = get_day_last_datetime(NowDatetime),
                    NextTimeList = [Datetime|next_time_util(Spec, NowDatetime, LastDatetime)],
                    insert_next_time_list(NextTimeList,State),
                    ets:insert(?ETS_NAME_TASKS, Task),
                    {ok, State};
                {error, Rsn} ->
                    {error, Rsn}
            end;
        false ->
            {error, task_exists}
    end.

get_day_last_datetime({Date, _}) ->
    {Date,{23,59,59}}.

next_time_util(Spec, NowDatetime, LastDatetime) ->
    case ecrontab_next_time:next_time(Spec, NowDatetime) of
        {ok, Datetime} when Datetime > LastDatetime ->
            [];
        {ok, Datetime} ->
            [Datetime|next_time_util(Spec, Datetime, LastDatetime)];
        _ ->
            []
    end.

insert_next_time_list(_NextTimeList,_State) ->
    %todo
    ok.

do_remove(Name, _Options, State) ->
    case ets:lookup(?ETS_NAME_TASKS, Name) of
        []  ->
            {error, no_such_task};
        _ ->
            ets:delete_object(?ETS_NAME_TASKS, Name),
            %todo
            {ok, State}
    end.

new_server_ets(N) ->
    ets:new(erlang:binary_to_atom(<<"ets_next_times_",(48+N)>>, utf8),
        [
            {keypos,#next_time.seconds},
            {write_concurrency, true},
            {read_concurrency, true}
        ]).