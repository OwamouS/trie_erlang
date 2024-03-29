-module(trie_tests).

-import(trie, [
new_trie/0,
trie_append/3,
trie_erase/2,
trie_search/2,
trie_filter/2,
trie_map/2,
trie_foldl/3,
trie_foldr/3,
trie_merge/2
]).

-include_lib("eunit/include/eunit.hrl").

%% Insert/Search
insert_search_test() ->
  _Trie = trie:trie_append("a", 42, trie:new_trie()),
  _Trie1 = trie:trie_append("bb", 43, _Trie),
  ?assertEqual(43, trie:trie_search("bb", _Trie1)),
  ?assertEqual(42, trie:trie_search("a", _Trie)).

%% Erase
erase_test() ->
  Trie1 = trie:trie_append("a", 42, trie:new_trie()),
  Trie2 = trie:trie_erase("a", Trie1),
  ?assertNotEqual(Trie1, Trie2).

%% Merge
merge_test() ->
  Trie1 = trie:trie_append("a", 42, trie:new_trie()),
  Trie2 = trie:trie_append("b", 43, trie:new_trie()),
  Trie3 = trie:trie_merge(Trie1, Trie2),
  ?assertEqual(42, trie:trie_search("a", Trie3)),
  ?assertEqual(43, trie:trie_search("b", Trie3)).

%% Filter
filter_test() ->
  Trie = trie:trie_append("a", 42, trie:new_trie()),
  Trie1 = trie:trie_append("b", -42, Trie),

  Trie2 = trie:trie_filter(fun(V) -> V > 0 end, Trie1),
  ?assertEqual(Trie, Trie2).

%% Map
map_test() ->
  Trie = trie:trie_append("a", 42, trie:new_trie()),
  Trie1 = trie:trie_append("b", -42, Trie),

  Trietest = trie:trie_append("a", 84, trie:new_trie()),
  Trietest2 = trie:trie_append("b", -84, Trietest),
  Trie2 = trie:trie_map(fun(V) -> V * 2 end, Trie1),
  ?assertEqual(Trietest2, Trie2).

%% Foldr/Foldl
foldr_foldl_test() ->
  Trie = trie:trie_append("a", 42, trie:new_trie()),

  Acc1 = trie:trie_foldr(fun(V, Acc) -> V + Acc end, 0, Trie),
  Acc2 = trie:trie_foldl(fun(Acc, V) -> V + Acc end, 0, Trie),

  Acc3 = trie:trie_foldl(fun(Acc, V) -> V + Acc end, 0, trie:trie_filter(fun(V) -> V > 0 end, Trie)),
  Acc4 = trie:trie_foldl(fun(Acc, V) -> V + Acc end, 0, trie:trie_map(fun(V) -> V * 2 end, Trie)),
  ?assertEqual(Acc1, 42),
  ?assertEqual(Acc2, 42),
  ?assertEqual(Acc3, 42),
  ?assertEqual(Acc4, 84).
