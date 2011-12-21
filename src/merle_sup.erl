-module(merle_sup).

-export([start_link/0, init/1]).

-export([start_child/1]).

-behaviour(supervisor).

start_link() ->
    Pid = case supervisor:start_link({local, ?MODULE}, ?MODULE, []) of
            {ok, P} -> P;
            {error, {already_started, P}} -> P
          end,
    merle_cluster:configure(),
    {ok, Pid}.

start_child(N) ->
    supervisor:start_child(?MODULE, [N]).

init([]) ->
    MCDSpec = {mcd, {merle_watcher, start_link, []},
                permanent, 5000, worker, dynamic},
    {ok, {{simple_one_for_one, 10, 10}, [MCDSpec]}}.
