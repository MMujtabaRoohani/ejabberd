%% Generated automatically
%% DO NOT EDIT: run `make options` instead

-module(mod_registered_group_roster_opt).

-export([name/1]).

-spec name(gen_mod:opts() | global | binary()) -> binary().
name(Opts) when is_map(Opts) ->
    gen_mod:get_opt(name, Opts);
name(Host) ->
    gen_mod:get_module_opt(Host, mod_registered_group_roster, name).

