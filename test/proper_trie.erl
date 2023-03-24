-module(proper_trie).
-author("SMash").


-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

-define(PROPERTY_TESTS_AMOUNT, 10).

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
%% API
-export([
  get_property_test_result/1]).

get_property_test_result(Property) -> proper:quickcheck(Property, [{numtests, ?PROPERTY_TESTS_AMOUNT}]).

%%Prop tests
prop_associative() ->
  ?FORALL({T1_K, T1_V, T2_K, T2_V, T3_K, T3_V},
    {string(), integer(), string(), integer(), string(), integer()},
    begin
      T1 = trie:trie_append(T1_K, T1_V, new_trie()),
      T2 = trie:trie_append(T2_K, T2_V, new_trie()),
      T3 = trie:trie_append(T3_K, T3_V, new_trie()),
      trie:trie_merge(trie:trie_merge(T2, T1),T3) == trie:trie_merge(trie:trie_merge(T1, T3), T2)
    end
  ).

prop_addition_tree() ->
  ?FORALL(
    {T1_K, T1_V, T2_K, T2_V},
    {string(), integer(), string(), integer()},
    begin
      Result = trie:trie_merge(trie:trie_append(T1_K, T1_V, new_trie()), trie:trie_append(T2_K, T2_V, new_trie())),
      InverseResult = trie:trie_merge(trie:trie_append(T2_K, T2_V, new_trie(), trie:trie_append(T1_K, T1_V, new_trie()))),
      Result == InverseResult
    end
  ).

associative_test() ->
  Property = prop_associative(),
  ?assert(get_property_test_result(Property)).

addition_test() ->
  Property = prop_addition_tree(),
  ?assert(get_property_test_result(Property)).
