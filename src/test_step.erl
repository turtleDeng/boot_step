-module(test_step).


-export([start/0, stop/0, stop_and_halt/0, status/0]).

-export([start/2, stop/1]).
-define(COPYRIGHT_MESSAGE, "Copyright (C) 2012-2012 Masget, Inc.").
-define(INFORMATION_MESSAGE, "Licensed masget.com/").
-define(ERTS_MINIMUM, "5.6.3").

-test_boot_step({pre_boot, [{description, "test boot start"}]}).

-test_boot_step({test_module,
                   [{description, "test module"},
                    {mfa,         {test_module, init, []}},
                    {requires,    pre_boot},
                    {enables,     external_infrastructure}
                    ]}).


-test_boot_step({test_function,
                   [{description, "test functions"},
                    {mfa,         {test_function, init, []}},
                    {requires,    test_module}]}).

-test_boot_step({external_infrastructure,
                   [{description, "external infrastructure ready"}]}).


-define(APPS, [test_step]).

start() ->
    try
        case application:load(test_step) of
            ok                                -> ok;
            {error, {already_loaded, test_step}} -> ok
        end,
        A = application_load_order(),
        %%io:format("A:~p~n", [A]),
        ok = misc:start_applications(A),
        ok
    after
        timer:sleep(100)
    end.

stop() ->
    io:format("Stopping test_step~n"),
    ok = misc:stop_applications(application_load_order()).

stop_and_halt() ->
    try
        stop()
    after
        misc:local_info_msg("Halting Erlang VM~n", []),
        init:stop()
    end,
    ok.

status() ->
    [{pid, list_to_integer(os:getpid())},
     {running_applications, application:which_applications(infinity)},
     {os, os:type()},
     {erlang_version, erlang:system_info(system_version)},
     {memory, erlang:memory()}] ++
     misc:filter_exit_map(
        fun ({Key, {M, F, A}}) -> {Key, erlang:apply(M, F, A)} end,
        [{vm_memory_high_watermark, {vm_memory_monitor,
                                     get_vm_memory_high_watermark, []}},

         {vm_memory_limit,          {vm_memory_monitor,
                                     get_memory_limit, []}}]).
%%--------------------------------------------------------------------

start(normal, []) ->
    io:format("start/2 function~n"),
    case erts_version_check() of
        ok ->
            {ok, SupPid} = test_step_sup:start_link(),
            true = register(test_step, self()),
            %%print_banner(),
            lists:foreach(
                fun(Step) -> 
                    ok = run_boot_step(Step)
                end, boot_steps()),
            io:format("~nbroker running~n"),
            {ok, SupPid};
        Error ->
            Error
    end.

stop(_State) ->
    ok.

%%---------------------------------------------------------------------------
%% application life cycle

application_load_order() ->
    ok = load_applications(),
    {ok, G} = misc:build_acyclic_graph(
                fun (App, _Deps) -> [{App, App}] end,
                fun (App,  Deps) -> [{Dep, App} || Dep <- Deps] end,
                [{App, app_dependencies(App)} ||
                    {App, _Desc, _Vsn} <- application:loaded_applications()]),
    true = digraph:del_vertices(
             G, digraph:vertices(G) -- digraph_utils:reachable(?APPS, G)),
    Result = digraph_utils:topsort(G),
    true = digraph:delete(G),
    Result.

load_applications() ->
    load_applications(queue:from_list(?APPS), sets:new()).

load_applications(Worklist, Loaded) ->
    case queue:out(Worklist) of
        {empty, _WorkList} ->
            ok;
        {{value, App}, Worklist1} ->
            case sets:is_element(App, Loaded) of
                true  -> load_applications(Worklist1, Loaded);
                false -> case application:load(App) of
                             ok                             -> ok;
                             {error, {already_loaded, App}} -> ok;
                             Error                          -> throw(Error)
                         end,
                         load_applications(
                           queue:join(Worklist1,
                                      queue:from_list(app_dependencies(App))),
                           sets:add_element(App, Loaded))
            end
    end.

app_dependencies(App) ->
    case application:get_key(App, applications) of
        undefined -> [];
        {ok, Lst} -> Lst
    end.

%%---------------------------------------------------------------------------
%% boot step logic

run_boot_step({StepName, Attributes}) ->
    Description = case lists:keysearch(description, 1, Attributes) of
                      {value, {_, D}} -> D;
                      false           -> StepName
                  end,
    case [MFA || {mfa, MFA} <- Attributes] of
        [] ->
            io:format("-- ~p~n", [Description]);
        MFAs ->
            io:format("starting ~p~n", [Description]),
            [try
                 apply(M,F,A)
             catch
                 _:Reason -> boot_error("FAILED~nReason: ~p~nStacktrace: ~p~n",
                                        [Reason, erlang:get_stacktrace()])
             end || {M,F,A} <- MFAs],
            io:format("done~n"),
            ok
    end.

boot_steps() ->
    sort_boot_steps(misc:all_module_attributes(test_boot_step)).

vertices(_Module, Steps) ->
    [{StepName, {StepName, Atts}} || {StepName, Atts} <- Steps].

edges(_Module, Steps) ->
    [case Key of
         requires -> {StepName, OtherStep};
         enables  -> {OtherStep, StepName}
     end || {StepName, Atts} <- Steps,
            {Key, OtherStep} <- Atts,
            Key =:= requires orelse Key =:= enables].

sort_boot_steps(UnsortedSteps) ->
    case misc:build_acyclic_graph(fun vertices/2, fun edges/2,
                                         UnsortedSteps) of
        {ok, G} ->
            %% Use topological sort to find a consistent ordering (if
            %% there is one, otherwise fail).
            SortedSteps = lists:reverse(
                            [begin
                                 {StepName, Step} = digraph:vertex(G, StepName),
                                 Step
                             end || StepName <- digraph_utils:topsort(G)]),
            digraph:delete(G),
            %% Check that all mentioned {M,F,A} triples are exported.
            case [{StepName, {M,F,A}} ||
                     {StepName, Attributes} <- SortedSteps,
                     {mfa, {M,F,A}}         <- Attributes,
                     not erlang:function_exported(M, F, length(A))] of
                []               -> SortedSteps;
                MissingFunctions -> boot_error(
                                      "Boot step functions not exported: ~p~n",
                                      [MissingFunctions])
            end;
        {error, {vertex, duplicate, StepName}} ->
            boot_error("Duplicate boot step name: ~w~n", [StepName]);
        {error, {edge, Reason, From, To}} ->
            boot_error(
              "Could not add boot step dependency of ~w on ~w:~n~s",
              [To, From,
               case Reason of
                   {bad_vertex, V} ->
                       io_lib:format("Boot step not registered: ~w~n", [V]);
                   {bad_edge, [First | Rest]} ->
                       [io_lib:format("Cyclic dependency: ~w", [First]),
                        [io_lib:format(" depends on ~w", [Next]) ||
                            Next <- Rest],
                        io_lib:format(" depends on ~w~n", [First])]
               end])
    end.

boot_error(Format, Args) ->
    io:format("BOOT ERROR: " ++ Format, Args),
    error_logger:error_msg(Format, Args),
    timer:sleep(1000),
    exit({?MODULE, failure_during_boot}).




%%---------------------------------------------------------------------------
%% misc

erts_version_check() ->
    FoundVer = erlang:system_info(version),
    case misc:version_compare(?ERTS_MINIMUM, FoundVer, lte) of
        true  -> ok;
        false -> {error, {erlang_version_too_old,
                          {found, FoundVer}, {required, ?ERTS_MINIMUM}}}
    end.

print_banner() ->
    {ok, Product} = application:get_key(id),
    {ok, Version} = application:get_key(vsn),
    ProductLen = string:len(Product),
    io:format("~n"
              "~s~n~s~n~s~n~s~n~n",
              [Product, string:right([$v|Version], ProductLen),
               ?COPYRIGHT_MESSAGE, ?INFORMATION_MESSAGE]),
    Settings = [{"node",           node()},
                {"app descriptor", app_location()},
                {"home dir",       home_dir()},
                {"config file(s)", config_files()},
                {"cookie hash",    misc:cookie_hash()},
                {"erlang version", erlang:system_info(version)}],
    DescrLen = 1 + lists:max([length(K) || {K, _V} <- Settings]),
    Format = fun (K, V) ->
                     io:format("~-" ++ integer_to_list(DescrLen) ++ "s: ~s~n",
                               [K, V])
             end,
    lists:foreach(fun ({"config file(s)" = K, []}) ->
                          Format(K, "(none)");
                      ({"config file(s)" = K, [V0 | Vs]}) ->
                          Format(K, V0), [Format("", V) || V <- Vs];
                      ({K, V}) ->
                          Format(K, V)
                  end, Settings),
    io:nl().

app_location() ->
    {ok, Application} = application:get_application(),
    filename:absname(code:where_is_file(atom_to_list(Application) ++ ".app")).

home_dir() ->
    case init:get_argument(home) of
        {ok, [[Home]]} -> Home;
        Other          -> Other
    end.

config_files() ->
    case init:get_argument(config) of
        {ok, Files} -> [filename:absname(filename:rootname(File, ".config") ++ ".config") || File <- Files];
        error       -> []
    end.