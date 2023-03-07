-module(trie).
-export([
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

-include("trie.hrl").

new_trie() ->
  #node{}.

trie_append([], V, Node) ->
  #node{value = V, children = Node#node.children};
trie_append([H | T], V, Node) ->
  Children = Node#node.children,
  case dict:find(H, Children) of
    error ->
      Leaf = trie_append(T, V, new_trie()),
      #node{children = dict:store(H, Leaf, Children)};
    {ok, Leaf} ->
      Leaf1 = trie_append(T, V, Leaf),
      #node{children = dict:store(H, Leaf1, Children)}
  end.

trie_erase([], Node) ->
  #node{value = undefined, children = Node#node.children};
trie_erase([H | T], Node) ->
  Children = Node#node.children,
  case dict:find(H, Children) of
    error ->
      Node;
    {ok, Leaf} ->
      Leaf1 = trie_erase(T, Leaf),
      case dict:is_empty(Leaf1#node.children) andalso Leaf1#node.value == undefined of
        true ->
          #node{children = dict:erase(H, Children)};
        false ->
          #node{children = dict:store(H, Leaf1, Children)}
      end
  end.

trie_search([], Node) ->
  Node#node.value;
trie_search([H | T], Node) ->
  Children = Node#node.children,
  case dict:find(H, Children) of
    error ->
      undefined;
    {ok, Leaf} ->
      trie_search(T, Leaf)
  end.

trie_filter(Predicate, Node) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      NewChildren = dict:fold(
        fun(Key, Leaf, Acc) ->
          FilteredLeaf = trie_filter(Predicate, Leaf),
          case dict:is_empty(FilteredLeaf#node.children) andalso FilteredLeaf#node.value == undefined of
            true -> Acc;
            false -> dict:store(Key, FilteredLeaf, Acc)
          end
        end, dict:new(), Children),
      #node{children = NewChildren};
    V ->
      case Predicate(V) of
        true -> #node{value = V, children = dict:new()};
        false -> #node{}
      end
  end.

trie_map(Transformer, Node) ->
  map(Transformer, Node, new_trie()).

map(Transformer, Node, NewNode) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      NewChildren = dict:map(fun(_, Leaf) -> map(Transformer, Leaf, new_trie()) end, Children),
      NewNode#node{children = NewChildren};
    V ->
      NewValue = Transformer(V),
      NewNode#node{value = NewValue}
  end.

foldl(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      dict:fold(fun(_, Leaf, Acc1) -> foldl(Fun, Acc1, Leaf) end, Acc, Children);
    V ->
      Fun(V, Acc)
  end.

foldr(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      Children = Node#node.children,
      dict:fold(fun(_, Leaf, Acc1) -> foldr(Fun, Acc1, Leaf) end, Acc, Children);
    V ->
      Fun(V, Acc)
  end.

trie_merge(Tree1, Tree2) ->
  #node{value = V, children = Children} = Tree1,
  #node{value = _Value2, children = Children2} = Tree2,
  case V of
    undefined ->
      NewChildren = dict:fold(
        fun(Key, Child2, Acc) ->
          case dict:find(Key, Children) of
            error ->
              dict:store(Key, Child2, Acc);
            {ok, Child1} ->
              dict:store(Key, trie_merge(Child1, Child2), Acc)
          end
        end, Children, Children2),
      #node{value = undefined, children = NewChildren};
    _ ->
      Tree1
  end.
