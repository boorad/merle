-module(merle_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for collecterl.
start(_Type, StartArgs) ->
    merle_sup:start_link(StartArgs).

%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for collecterl.
stop(_State) ->
    ok.
