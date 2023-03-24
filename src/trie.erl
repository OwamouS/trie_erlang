-module(trie).
-export([
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

-include("trie.hrl").

new_trie() -> #node{}.

trie_append([], V, Node) -> #node{value = V, leaf = Node#node.leaf};
trie_append([H | T], V, Node) ->
  TrieDict_Children = Node#node.leaf,
  case dict:find(H, TrieDict_Children) of
    {ok, Leaf} ->
      Leaf1 = trie_append(T, V, Leaf),
      #node{leaf = dict:store(H, Leaf1, TrieDict_Children)};
    error ->
      Leaf = trie_append(T, V, new_trie()),
      #node{leaf = dict:store(H, Leaf, TrieDict_Children)}
  end.

trie_erase([], Node) ->
  #node{value = undefined, leaf = Node#node.leaf};
trie_erase([H | T], Node) ->
  TrieDict_Children = Node#node.leaf,
  case dict:find(H, TrieDict_Children) of
    {ok, Leaf} ->
      Leaf1 = trie_erase(T, Leaf),
      case dict:is_empty(Leaf1#node.leaf) andalso Leaf1#node.value == undefined of
        true -> #node{leaf = dict:erase(H, TrieDict_Children)};
        false -> #node{leaf = dict:store(H, Leaf1, TrieDict_Children)}
      end;
    error -> Node
  end.

trie_search([], Node) -> Node#node.value;

trie_search([H | T], Node) ->
  TrieDict_Children = Node#node.leaf,
  case dict:find(H, TrieDict_Children) of
    {ok, Leaf} ->
      trie_search(T, Leaf);
    error ->
      undefined
  end.

trie_filter(Pred, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.leaf,
      NewChildren = dict:fold(
        fun(K, Leaf, Acc) ->
          FilteredLeaf = trie_filter(Pred, Leaf),
          case dict:is_empty(FilteredLeaf#node.leaf) andalso FilteredLeaf#node.value == undefined of
            true -> Acc;
            false -> dict:store(K, FilteredLeaf, Acc)
          end
        end, dict:new(), TrieDict_Children),
      #node{leaf = NewChildren};
    V ->
      case Pred(V) of
        true -> #node{value = V, leaf = dict:new()};
        false -> #node{}
      end
  end.

trie_merge(Tree1, Tree2) ->
  #node{value = V, leaf = TrieDict_Children} = Tree1,
  #node{value = _V2, leaf = TrieDict_Children2} = Tree2,
  NewChildren = dict:fold(
    fun(K, Leaf2, Acc) ->
      case dict:find(K, TrieDict_Children) of
        {ok, Leaf} -> dict:store(K, trie_merge(Leaf, Leaf2), Acc);
        error -> dict:store(K, Leaf2, Acc)
      end
    end, TrieDict_Children, TrieDict_Children2),
  case V of
    undefined -> #node{value = _V2, leaf = NewChildren};
    _ -> #node{value = _V2, leaf = NewChildren}
  end.

trie_map(Func, Leaf) -> map(Func, Leaf, new_trie()).

map(Func, Node, NewNode) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.leaf,
      NewChildren = dict:map(fun(_, Leaf) -> map(Func, Leaf, new_trie()) end, TrieDict_Children),
      NewNode#node{leaf = NewChildren};
    V ->
      NewV = Func(V),
      NewNode#node{value = NewV}
  end.

trie_foldl(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.leaf,
      dict:fold(fun(_, Leaf, Acc1) -> trie_foldl(Fun, Acc1, Leaf) end, Acc, TrieDict_Children);
    V -> Fun(V, Acc)
  end.

trie_foldr(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.leaf,
      dict:fold(fun(_, Leaf, Acc1) -> trie_foldr(Fun, Acc1, Leaf) end, Acc, TrieDict_Children);
    V -> Fun(V, Acc)
  end.
