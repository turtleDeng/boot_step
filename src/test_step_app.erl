-module(test_step_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	io:format("start test_step_app ~n"),
    test_step_sup:start_link().

stop(_State) ->
    ok.
