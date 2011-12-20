-module(merle_app).

-behaviour(application).
-export([start/2,stop/1]).


%% @spec start(_Type, _StartArgs) -> ServerRet
%% @doc application start callback for merle.
start(_Type, _StartArgs) ->
  merle_sup:start_link().


%% @spec stop(_State) -> ServerRet
%% @doc application stop callback for merle.
stop(_State) ->
  ok.
