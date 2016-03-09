-module(ecrontab_clock_tick).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
    start_link/0
]).

-record(state, {}).
-define(START_TASK_TIMER, erlang:start_timer(1000, self(), tick)).

%% ====================================================================
%% API
%% ====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).


%% ====================================================================
%% callback API
%% ====================================================================

init(_Args) ->
    process_flag(trap_exit, true),
    ?START_TASK_TIMER,
    {ok, #state{}}.

handle_call(_Msg, _From, State) ->
    {reply, noknow, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({timeout, _Ref, tick}, State) ->
    ?START_TASK_TIMER,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_Old, State, _Extra) ->
    {ok, State}.