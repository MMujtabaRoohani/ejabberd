%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_push_modified_opt).

-export([host/1]).

-spec host(gen_mod:opts() | global | binary()) -> binary().
host(Opts) when is_map(Opts) ->
    gen_mod:get_opt(host, Opts);
host(Host) ->
    gen_mod:get_module_opt(Host, mod_push_modified, host).

