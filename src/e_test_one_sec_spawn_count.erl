-module(e_test_one_sec_spawn_count).

-export([
    loop_fun/0,
    loop_spawn/1
]).

loop_fun() ->
    erlang:start_timer(1000, self(), tick),
    io:format("start:~p~n",[os:timestamp()]),
    loop_fun(0),
    io:format("end:~p~n",[os:timestamp()]).
loop_fun(N) ->
    receive
        _ ->
            io:format("count:~p~n",[N])
    after 0 ->
        spawn(fun() -> ok end),
        loop_fun(N+1)
    end.

loop_spawn(N) ->
    P = self(),
    T1 = erlang:monotonic_time(),
    spawn(fun() -> loop_spawn(0,N), P ! ok end),
    receive
        ok ->
            ok
    end,
    T2 = erlang:monotonic_time(),
    Time = erlang:convert_time_unit(T2 - T1, native, micro_seconds),
    io:format("loop_spawn seconds:~p~n",[Time/1000000]),
    PList = erlang:processes(),
    {SendTime, _} = timer:tc(fun() -> loop_send(PList) end),
    SendTime/1000000.
loop_spawn(N,N) ->
    ok;
loop_spawn(N1,N2) ->
    spawn(fun loop_spawn_do/0),
    loop_spawn(N1+1,N2).
loop_spawn_do() ->
    receive
        test_stop ->
            ok
    end.

loop_send([Pid|PList]) ->
    Pid ! test_stop,
    loop_send(PList);
loop_send([]) ->
    ok.
