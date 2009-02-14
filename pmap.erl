-module(pmap).
-export([
	 test/1,
	 pmap/2
	]).

%%% Public interface.

test(Count) ->
    %% {Usecs, Res} = timer:tc(?MODULE, pmap, [fun fac/1, lists:seq(1, Count)]),
    {Usecs, Res} = timer:tc(?MODULE, pmap, [fun flake/1, lists:seq(1, Count)]),
    io:format("## Took ~w seconds. Last: ~p~n", [Usecs / 1000000, lists:last(Res)]),
    ok.

pmap(Fun, List) when is_function(Fun), is_list(List) ->
    Master = self(),
    Nodes = [node() | nodes()],
    {Pids, _} = lists:mapfoldl(fun (Item, [Node | Rest]) ->
				       Pid = run(Node, Master, Fun, Item),
				       {Pid, Rest};
				   (Item, []) ->
				       [Node | Rest] = Nodes,
				       Pid = run(Node, Master, Fun, Item),
				       {Pid, Rest}
			       end, Nodes, List),
    gather(Pids).

%%% Implementation.

gather([Pid | Pids]) ->
    receive
        {Pid, Ret} ->
	    [Ret | gather(Pids)]
    end;
gather([]) ->
    [].

run(Node, Master, Fun, Item) ->
    spawn(Node, fun () ->
			Result = (catch Fun(Item)),
			Master ! {self(), Result}
		end).

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
    
%%% Cement
%%% Tomasova dalsi modifikace
%%% volovina