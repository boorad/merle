-module(merle_cluster).

-export([configure/0, configure/1, configure/2]).

configure() ->
  {ok, Conf} = get_conf(),
  MemcachedHosts = proplists:get_value(memcached_hosts, Conf),
  ConnectionsPerHost = proplists:get_value(connections_per_host, Conf),
  configure(MemcachedHosts, ConnectionsPerHost).

configure(ConnectionsPerHost) ->
  {ok, Conf} = get_conf(),
  MemcachedHosts = proplists:get_value(memcached_hosts, Conf),
  configure(MemcachedHosts, ConnectionsPerHost).

configure(MemcachedHosts, ConnectionsPerHost) ->
  clear_existing_connections(),
  SortedMemcachedHosts = lists:sort(MemcachedHosts),
  DynModuleBegin = "-module(merle_cluster_dynamic).
         -export([get_server/1]).
         get_server(ClusterKey) -> N = erlang:phash2(ClusterKey, ~p),
                                   do_get_server(N).
                                   ",
  DynModuleMap = "do_get_server(~p) -> {\"~s\", ~p}; ",
  DynModuleEnd = "do_get_server(_N) -> throw({invalid_server_slot, _N}).\n",
  ModuleString = lists:flatten([
    io_lib:format(DynModuleBegin, [length(SortedMemcachedHosts)]),
    index_map(fun([Host, Port], I) ->
                  io_lib:format(DynModuleMap, [I-1, Host, Port])
              end, SortedMemcachedHosts),
    DynModuleEnd
  ]),
  {M, B} = dynamic_compile:from_string(ModuleString),
  code:load_binary(M, "", B),
  lists:foreach(fun([Host, Port]) ->
                    load_workers(ConnectionsPerHost, Host, Port)
                end, SortedMemcachedHosts).

%%
%% internal
%%

index_map(F, List) ->
  {Map, _} = lists:mapfoldl(fun(X, Iter) ->
    {F(X, Iter), Iter +1}
  end, 1, List),
  Map.

load_workers(ConnectionsPerHost, Host, Port) ->
  lists:foreach(fun(_) ->
    supervisor:start_child(merle_sup, [[Host, Port]])
  end, lists:seq(1, ConnectionsPerHost)).

clear_existing_connections() ->
  Children = supervisor:which_children(merle_sup),
  close(Children).

close([]) -> ok;
close([{_, ChildPid, _, _}|Rest]) ->
  supervisor:terminate_child(merle_sup, ChildPid),
  supervisor:delete_child(merle_sup, ChildPid),
  close(Rest).

get_conf() ->
  Filename = code:priv_dir(merle) ++ "/merle.conf",
  case file:consult(Filename) of
    {ok, Terms} -> {ok, Terms};
    {error, enoent} -> throw({error, no_config_file})
  end.
