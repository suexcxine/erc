-module(erc).
-export([get_cluster_slots/2, parse_cluster_slots/1]).
-export([get_node_index/2, get_node_by_slot/2]).
-export([get_key_from_command/1, calc_slot_by_key/1, get_hash_tag/1]).

get_cluster_slots([], _CallbackMod) ->
    [];
get_cluster_slots([{Host, Port}|T], CallbackMod) ->
    case apply(CallbackMod, get_cluster_slots, [Host, Port]) of
        {ok, ClusterInfo} -> parse_cluster_slots(ClusterInfo);
        _ -> get_cluster_slots(T, CallbackMod)
    end.

parse_cluster_slots(ClusterInfo) ->
    parse_cluster_slots(ClusterInfo, 1, []).

parse_cluster_slots([[StartSlot, EndSlot | [[MasterAddress, MasterPort | _] | _]] | T], Index, Acc) ->
    Node = {binary_to_integer(StartSlot), binary_to_integer(EndSlot), Index,
        MasterAddress, binary_to_integer(MasterPort)},
    parse_cluster_slots(T, Index + 1, [Node | Acc]);
parse_cluster_slots([], _Index, Acc) ->
    lists:reverse(Acc).

get_node_index(Mapping, Command) ->
    Key = get_key_from_command(Command),
    Slot = calc_slot_by_key(Key),
    Index = get_node_by_slot(Mapping, Slot),
    Index.

get_node_by_slot([], _Slot) ->
    undefined;
get_node_by_slot([{Start, End, Index, _, _}], Slot) when Start =< Slot andalso Slot =< End ->
    Index;
get_node_by_slot([_|T], Slot) ->
    get_node_by_slot(T, Slot).

% Commands not suitable for cluster mode will NOT succeed
get_key_from_command([_Command, Key|_]) ->
    Key.

calc_slot_by_key(Key) ->
    case get_hash_tag(Key) of
        undefined -> erc_hash:hash(Key);
        HashTag -> erc_hash:hash(HashTag)
    end.

get_hash_tag(Key) ->
    case re:run(Key, <<"{(.*)}">>, [{capture, all_but_first, binary}]) of
        {match, [HashTag]} when HashTag =/= <<>> -> HashTag;
        _ -> undefined
    end.

