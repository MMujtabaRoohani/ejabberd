%%%----------------------------------------------------------------------
%%% File    : mod_registered_group_roster.erl
%%% Author  : Mujtaba Roohani <mujtaba.roohani@gmail.com>
%%% Purpose : Create a group of registered users in the roster.
%%% Created :  14 Oct 2022 by Mujtaba Roohani <mujtaba.roohani@gmail.com>
%%%
%%% Copyright (C) 2022 Mujtaba Roohani
%%%
%%% This program is free software: you can redistribute it and/or modify
%%% it under the terms of the GNU General Public License as published by
%%% the Free Software Foundation, either version 3 of the License, or
%%% (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
%%% GNU General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program.  If not, see <https://www.gnu.org/licenses/>
%%%----------------------------------------------------------------------

-module(mod_registered_group_roster).
-author('mujtaba.roohani@gmail.com').
-behaviour(gen_mod).

-export([start/2, stop/1, reload/3, process_item/2, mod_opt_type/1, mod_options/1, mod_doc/0, depends/2]).

-include("logger.hrl").

-include_lib("xmpp/include/xmpp.hrl").

-include("mod_roster.hrl").

-include("ejabberd_http.hrl").

-include("ejabberd_web_admin.hrl").

-include("mod_shared_roster.hrl").

-include("translate.hrl").

start(Host, Opts) ->
    ejabberd_hooks:add(roster_process_item, Host, ?MODULE,
		       process_item, 50),
    ejabberd_hooks:add(register_user, Host, ?MODULE,
		       register_user, 50),
    ejabberd_hooks:add(remove_user, Host, ?MODULE,
		       remove_user, 50).

stop(Host) ->
    ejabberd_hooks:delete(roster_process_item, Host,
			  ?MODULE, process_item, 50),
    ejabberd_hooks:delete(register_user, Host, ?MODULE,
			  register_user, 50),
    ejabberd_hooks:delete(remove_user, Host, ?MODULE,
			  remove_user,
			  50).

reload(_, _, _) ->
    ok.

depends(_Host, _Opts) ->
    [].

%% This function rewrites the roster entries when moving or renaming
%% them in the user contact list.
-spec process_item(#roster{}, binary()) -> #roster{}.
process_item(RosterItem, Host) ->
	DefaultGroupName = mod_registered_group_roster_opt:name(Host),
    {UserTo, ServerTo, _} = RosterItem#roster.jid,
    USToExists = ejabberd_auth:user_exists(UserTo, ServerTo),
    case USToExists of
      false -> RosterItem#roster{groups=lists:subtract(RosterItem#roster.groups,
			      [DefaultGroupName])};
      true -> RosterItem#roster{groups=[DefaultGroupName]}
    end.

mod_opt_type(name) ->
    econf:binary().

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(_) ->
    [{name, ?T("registered")}].

mod_doc() ->
    #{desc =>
	  [?T("This module automatically create a group for all contacts of a user"
	      "who are registered on the local server"), "",
	   ?T("The big advantage of this feature is that end user would be "
		  "able to distinguish in their contact list users who are registered "
		  "from the users who are not."), "",
	   ?T("This module depends on _`mod_roster`_. "
	      "If not enabled, roster queries will return 503 errors.")],
      opts =>
          [{name,
            #{value => "string()",
              desc =>
                  ?T("The name of the default group name. The default is 'registered'.")}}]}.
