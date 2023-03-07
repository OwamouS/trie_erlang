-module(prop_tests).

-include_lib("proper/include/proper.hrl").
-include_lib("stdlib/include/assert.hrl").

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

-export([
  prop_trie_append/0,
  prop_trie_remove/0,
  prop_trie_map/0,
  prop_trie_filter/0,
  prop_trie_merge/0,
  prop_trie_is_monoid/0
]).

prop_trie_remove() ->
  ?FORALL({Key, V}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000, "hello", "one", "two"])
  },
    begin
      Tree0 = new_trie(),
      Tree1 = trie_append(Key, V, Tree0),
      Tree2 = trie_erase(Key, Tree1),
      Status = trie_search(Key, Tree2),
      case Status of
        undefined -> true;
        V -> false
      end
    end).

prop_trie_append() ->
  ?FORALL({Key, V}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000, "hello", "one", "two"])
  },
    begin
      Tree0 = new_trie(),
      Tree1 = trie_append(Key, V, Tree0),
      Status = trie_search(Key, Tree1),
      case Status of
        V -> true;
        undefined -> false
      end
    end).

prop_trie_map() ->
  ?FORALL({Key, V}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000])
  },
    begin
      Tree0 = new_trie(),
      Tree1 = trie_append(Key, V, Tree0),
      Tree2 = trie_map(fun(X) -> X * 2 end, Tree1),
      Status = trie_search(Key, Tree2),
      ExpectedValue = V * 2,
      case Status of
        ExpectedValue -> true;
        undefined -> false
      end
    end).

prop_trie_filter() ->
  ?FORALL({Key, V}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([10, 20, 30, 40, 50, 100, 200, 300, 400, 450, 500, 600, 700, 800, 950, 1000])
  },
    begin
      Tree0 = new_trie(),
      Tree1 = trie_append(Key, V, Tree0),
      Tree2 = trie_filter(fun(X) -> X > 500 end, Tree1),
      Status = trie_search(Key, Tree2),
      case Status of
        V -> case V > 500 of
                   true -> true;
                   false -> false
                 end;
        undefined -> case V > 500 of
                       true -> false;
                       false -> true
                     end
      end
    end).

prop_trie_merge() ->
  ?FORALL({Key, V}, {
    oneof(["q", "qb", "qbc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([1, 2, 3, 4, 1000])
  },
    begin
      Node1 = trie_append(Key ++ "a", V, new_trie()),
      Node2 = trie_append(Key ++ "b", V + 5, Node1),
      Node3 = trie_append(Key ++ "c", V + 10, Node2),

      Node4 = trie_append(Key ++ "b", V + 15, new_trie()),
      Node5 = trie_append(Key ++ "d", V + 20, Node4),

      Merged = trie_merge(Node3, Node5),

      Equals1 = V =:= trie_search(Key ++ "a", Merged),
      Equals2 = V + 5 =:= trie_search(Key ++ "b", Merged),
      Equals3 = V + 10 =:= trie_search(Key ++ "c", Merged),
      Equals4 = V + 20 =:= trie_search(Key ++ "d", Merged),
      Status = Equals1 == true andalso Equals2 == true andalso Equals3 == true andalso Equals4 == true,

      case Status of
        true -> true;
        false -> false
      end
    end).

prop_trie_is_monoid() ->
  ?FORALL({Key, V}, {
    oneof(["a", "ab", "abc", "abcd", "foo", "foobar", "foofoo", "barbar", "qweqweqweq", "asdas", "zxczxc", "qqq"]),
    oneof([10, 20, 30, 40, 50, 100, 200, 300, 400, 450, 500, 600, 700, 800, 950, 1000])
  },
    begin
      Tree0 = new_trie(),
      Tree1 = trie_append(Key, V, Tree0),
      Tree2 = trie_append(Key ++ "zxc", V, Tree0),
      Tree3 = trie_append(Key, V + 50, Tree0),

      Merge1 = trie_merge(Tree1, trie_merge(Tree2, Tree3)),
      Merge2 = trie_merge(trie_merge(Tree1, Tree2), Tree3),

      Merge3 = trie_merge(Tree2, trie_merge(Tree1, Tree3)),
      Merge4 = trie_merge(trie_merge(Tree2, Tree3), Tree1),

      Merge5 = trie_merge(Tree2, trie_merge(Tree1, Tree3)),
      Merge6 = trie_merge(trie_merge(Tree2, Tree3), Tree1),

      Status = Merge1 =:= Merge2 andalso Merge3 =:= Merge4 andalso Merge5 =:= Merge6,
      case Status of
        true -> true;
        false -> false
      end
    end).
