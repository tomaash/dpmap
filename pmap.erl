-module(pmap).
-export([
	 test/1,
	 pmap/2
	]).

%%% Public interface.

test(Count) ->
    %% {Usecs, Res} = timer:tc(?MODULE, pmap, [fun flake/1, lists:seq(1, Count)]),
    {Usecs, Res} = timer:tc(?MODULE, pmap, [fun fib/1, lists:seq(1, Count)]),
    io:format("## Took ~w seconds. All: ~p~n", [Usecs / 1000000, Res]),
    ok.

pmap(Fun, List) when is_function(Fun), is_list(List) ->
    Master = self(),
    Workers = lists:map(fun (Node) ->
				proc_lib:spawn_link(Node, fun () ->
								  control(Master)
							  end)
			end, [node() | nodes()]),
    {Refs, _} = lists:mapfoldl(fun (Item, [Worker | Rest]) ->
				       Ref = make_ref(),
				       Worker ! {do, Ref, Fun, Item},
				       {Ref, Rest};
				   (Item, []) ->
				       [Worker | Rest] = Workers,
				       Ref = make_ref(),
				       Worker ! {do, Ref, Fun, Item},
				       {Ref, Rest}
			       end, Workers, List),
    lists:foreach(fun (Worker) ->
			  Worker ! stop
		  end, Workers),
    Res = gather_tasks(Refs),
    gather_workers(Workers),
    Res.

%%% Implementation.

gather_tasks([Ref | Refs]) ->
    receive
        {Ref, Ret} ->
	    [Ret | gather_tasks(Refs)]
    end;
gather_tasks([]) ->
    [].

gather_workers([Pid | Pids]) ->
    receive
	{Pid, done} ->
	    gather_workers(Pids)
    end;
gather_workers([]) ->
    ok.

control(Master) ->
    receive
	{do, Ref, Fun, Arg} ->
	    Res = (catch Fun(Arg)),
	    Master ! {Ref, Res},
	    control(Master);
	stop ->
	    Master ! {self(), done}
    end.

%%% Utilities.

timestamp() ->
    Datetime = calendar:universal_time(),
    calendar:datetime_to_gregorian_seconds(Datetime).

fib(1) ->
    1;
fib(2) ->
    1;
fib(X) ->
    fib(X - 1) + fib(X - 2).

fac(1) ->
    1;
fac(N) when is_integer(N), N > 1 ->
    N * fac(N - 1).

flake(N) ->
    timer:sleep(N),
    ok.
