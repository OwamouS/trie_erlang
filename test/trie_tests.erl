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

trie_append_erase_test() ->
  % Test empty trie
  ?assertEqual(undefined, trie:trie_search([], trie:new_trie())),
  %
  T1 = trie:trie_append("a", 1, trie:new_trie()),
  ?assertEqual(1, trie:trie_search("a", T1)),
  T2 = trie:trie_append("b", 2, T1),
  ?assertEqual(2, trie:trie_search("b", T2)),
  %%
  T3 = trie:trie_erase("b", T2),
  ?assertEqual(undefined, trie:trie_search("b", T3)).

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
