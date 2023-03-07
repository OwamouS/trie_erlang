-module(trie_tests).

-import(trie, [
new_trie/0,
trie_append/3,
trie_erase/2,
trie_search/2,
trie_filter/2,
trie_map/2,
foldl/3,
foldr/3,
trie_merge/2
]).

-include_lib("eunit/include/eunit.hrl").

trie_test() ->
  % Test empty trie
  ?assertEqual(undefined, trie:trie_search([], trie:new_trie())),

  % Test single value
  T1 = trie:trie_append("hello", 50, trie:new_trie()),
  ?assertEqual(50, trie:trie_search("hello", T1)),
  ?assertEqual(undefined, trie:trie_search("world", T1)),

  % Test multiple values
  T2 = trie:trie_append("world", 100, T1),
  T3 = trie:trie_append("bar", 200, T2),
  T4 = trie:trie_append("foobar", 300, T3),
  T5 = trie:trie_append("foofoo", 400, T4),

  ?assertEqual(100, trie:trie_search("world", T5)),
  ?assertEqual(200, trie:trie_search("bar", T5)),
  ?assertEqual(300, trie:trie_search("foobar", T5)),
  ?assertEqual(400, trie:trie_search("foofoo", T5)),
  ?assertEqual(undefined, trie:trie_search("fo", T5)),
  ?assertEqual(undefined, trie:trie_search("foob", T5)),

  %% remove test
  T6 = trie:trie_erase("foofoo", T5),
  ?assertEqual(undefined, trie:trie_search("foofoo", T6)),

  T7 = trie:trie_erase("world", T6),
  ?assertEqual(undefined, trie:trie_search("world", T7)),

  %% filter the trie for words starting with "foo"
  BiggerThanTwo = fun(Val) -> Val > 250 end,
  FilteredTrie = trie:trie_filter(BiggerThanTwo, T5),

  ?assertEqual(undefined, trie:trie_search("world", FilteredTrie)),
  ?assertEqual(undefined, trie:trie_search("bar", FilteredTrie)),
  ?assertEqual(300, trie:trie_search("foobar", FilteredTrie)),
  ?assertEqual(400, trie:trie_search("foofoo", FilteredTrie)),

  %% map tests
  Fun = fun(X) -> X * 2 end,
  T11 = trie:trie_map(Fun, T5),
  ?assertEqual(200, trie:trie_search("world", T11)),
  ?assertEqual(400, trie:trie_search("bar", T11)),
  ?assertEqual(600, trie:trie_search("foobar", T11)),
  ?assertEqual(800, trie:trie_search("foofoo", T11)),

  %% foldr and foldl tests
  Sum = fun(V, Acc) -> V + Acc end,
  FoldlSum = trie:foldl(Sum, 0, T5),
  FoldrSum = trie:foldr(Sum, 0, T5),
  ?assertEqual(1050, FoldlSum),
  ?assertEqual(1050, FoldrSum).

trie_merge_test() ->
  Node1 = trie_append("foo", 1, new_trie()),
  Node2 = trie_append("bar", 2, Node1),
  Node3 = trie_append("baz", 3, Node2),

  % create tree 2
  Node4 = trie_append("foo", 4, new_trie()),
  Node5 = trie_append("qux", 5, Node4),

  % merge the two trees
  Merged = trie_merge(Node3, Node5),

  % check that the merged tree has all the expected values
  ?assertEqual(1, trie_search("foo", Merged)),
  ?assertEqual(2, trie_search("bar", Merged)),
  ?assertEqual(3, trie_search("baz", Merged)),
  ?assertEqual(5, trie_search("qux", Merged)).

trie_monoid_test() ->
  ?assertEqual(new_trie(), trie_merge(new_trie(), new_trie())),
  ?assertEqual(trie_merge(new_trie(), new_trie()), new_trie()),
  Tree1 = trie_append("hello", "world", new_trie()),
  Tree2 = trie_append("goodbye", "everyone", new_trie()),
  Tree3 = trie_append("morning", "sir", new_trie()),
  ?assertEqual(trie_merge(Tree1, trie_merge(Tree2, Tree3)), trie_merge(trie_merge(Tree1, Tree2), Tree3)),
  ?assertEqual(trie_merge(Tree2, trie_merge(Tree1, Tree3)), trie_merge(trie_merge(Tree1, Tree2), Tree3)),
  ?assertEqual(trie_merge(Tree3, trie_merge(Tree1, Tree2)), trie_merge(trie_merge(Tree1, Tree2), Tree3)).
