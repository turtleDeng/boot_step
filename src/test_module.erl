-module (test_module).

-export ([init/0]).

init() ->
	io:format("test_module init~n"),
	ok.