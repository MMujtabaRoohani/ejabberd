%%%----------------------------------------------------------------------
%%% File    : mod_push_modified.erl
%%% Author  : Mujtaba Roohani <mujtaba@tilismtech.com>
%%% Purpose : Push Notifications (XEP-0357)
%%% Created : 7 Oct 2022 by Mujtaba Roohani <mujtaba@tilismtech.com>
%%%----------------------------------------------------------------------

-module(mod_push_modified).
-author('mujtaba@tilismtech.com').
-behaviour(gen_mod).

%% gen_mod callbacks.
-export([start/2, stop/1, reload/3, mod_opt_type/1, mod_options/1, depends/2]).
-export([mod_doc/0]).
%% ejabberd_hooks callbacks.
-export([offline_message/1]).

-include("logger.hrl").
-include_lib("xmpp/include/xmpp.hrl").
-include("translate.hrl").

%%--------------------------------------------------------------------
%% gen_mod callbacks.
%%--------------------------------------------------------------------
-spec start(binary(), gen_mod:opts()) -> ok.
start(Host, _) ->
    register_hooks(Host).

-spec stop(binary()) -> ok.
stop(Host) ->
    unregister_hooks(Host).

-spec reload(binary(), gen_mod:opts(), gen_mod:opts()) -> ok.
reload(_, _, _) ->
    ok.

-spec depends(binary(), gen_mod:opts()) -> [{module(), hard | soft}].
depends(_Host, _Opts) ->
    [].

-spec mod_opt_type(atom()) -> econf:validator().
mod_opt_type(host) ->
    econf:host().

-spec mod_options(binary()) -> [{atom(), any()}].
mod_options(Host) ->
    [{host, <<"push.", Host/binary>>}].

mod_doc() ->
    #{desc =>
          ?T("This module is an alternate to the mod_webhook offered by ejabberd BE."),
      opts =>
          [{host,
            #{value => "Host",
              desc =>
                  ?T("This option defines the host to receive offline messages from the service. "
                     "If the 'host' option is not specified, the host will be "
                     "the hostname of the virtual host with the prefix \"push.\". ")}}]}.

%%--------------------------------------------------------------------
%% Register/unregister hooks.
%%--------------------------------------------------------------------
-spec register_hooks(binary()) -> ok.
register_hooks(Host) ->
    ejabberd_hooks:add(offline_message_hook, Host, ?MODULE,
		       offline_message, 55).

-spec unregister_hooks(binary()) -> ok.
unregister_hooks(Host) ->
    ejabberd_hooks:delete(offline_message_hook, Host, ?MODULE,
			  offline_message, 55).

%%--------------------------------------------------------------------
%% Hook callbacks.
%%--------------------------------------------------------------------
-spec offline_message({any(), message()}) -> {any(), message()}.
offline_message({offlined,
		 #message{to = To} = Pkt} = Acc) ->
    ?DEBUG("Notifying ~ts of offline message", [jid:encode(To)]),
	notify(To, Pkt),
    Acc;
offline_message(Acc) ->
    Acc.

%%--------------------------------------------------------------------
%% Generate push notifications.
%%--------------------------------------------------------------------
-spec notify(jid(), xmpp_element() | xmlel() | none) -> ok.
notify(#jid{lserver = LServer} = To, Pkt) ->
    UnWrappedPkt = unwrap_message(Pkt),
	DelayedPkt = add_delay_info(UnWrappedPkt, LServer, undefined),
	Id = p1_rand:get_string(),
	PushServer = mod_push_modified_opt:host(LServer),
	WrappedPacket = wrap(DelayedPkt, <<"urn:xmpp:push:nodes:messages">>, Id),
	ejabberd_router:route(xmpp:set_from_to(WrappedPacket, To, jid:make(PushServer))).

%%--------------------------------------------------------------------
%% Internal functions.
%%--------------------------------------------------------------------
-spec unwrap_message(Stanza) -> Stanza when Stanza :: stanza() | none.
unwrap_message(#message{meta = #{carbon_copy := true}} = Msg) ->
    misc:unwrap_carbon(Msg);
unwrap_message(#message{type = normal} = Msg) ->
    case misc:unwrap_mucsub_message(Msg) of
	#message{} = InnerMsg ->
	    InnerMsg;
	false ->
	    Msg
    end;
unwrap_message(Stanza) ->
    Stanza.

-spec wrap(stanza(), binary(), binary()) -> message().
wrap(Packet, Node, Id) ->
    #message{
	id = Id,
	sub_els = [#ps_event{
	    items = #ps_items{
		node = Node,
		items = [#ps_item{
		    id = Id,
		    sub_els = [Packet]}]}}]}.

-spec add_delay_info(message(), binary(),
		     undefined | erlang:timestamp()) -> message().
add_delay_info(Packet, LServer, TS) ->
    NewTS = case TS of
		undefined -> erlang:timestamp();
		_ -> TS
	    end,
    Packet1 = xmpp:put_meta(Packet, from_offline, true),
    misc:add_delay_info(Packet1, jid:make(LServer), NewTS,
			<<"Offline storage">>).