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

% Adds or updates an object with the given key and returns the new trie.
%store ([], Object, {TrieDict, _}) ->
%	{TrieDict, Object};
%store ([Elem | StringTail], Object, {TrieDict, Slot}) ->
%	case orddict:find (Elem, TrieDict) of
%		{ok, Subtrie} ->
%			{orddict:store (Elem, store (StringTail, Object, Subtrie), TrieDict), Slot};
%		error ->
%			{orddict:store (Elem, store (StringTail, Object, new ()), TrieDict), Slot}
%	end.
%
% Get the next trie from the key.
%next ([], {TrieDict, Slot}) ->
%	{TrieDict, Slot};
%next ([Elem | StringTail], {TrieDict, _}) ->
%	case orddict:find (Elem, TrieDict) of
%		{ok, Subtrie} -> 
%			next (StringTail, Subtrie);
%		error -> undefined
%	end.
%
%% search(Key)
%search([]) ->
%  value;
%search([H|T]) ->
%  case H of
%    0 ->
%      search(T, 0);
%    1 ->
%      search(T, 1)
%  end.
%
%search([], N) ->
%  element(N);
%search([H|T], N) ->
%  case H of
%    0 ->
%      search(T, 0);
%    1 ->
%      search(T, 1)
%  end.
%
%% erase(Key)
%erase([]) ->
%  ok;
%erase([H|T]) ->
%  case H of
%    0 ->
%      {0, erase(T)};
%    1 ->
%      {1, erase(T)}
%  end.
%
%% merge(Trie1, Trie2)
%merge(T1, T2) ->
%  case T1 of
%    {0, V1} ->
%      case T2 of
%        {0, V2} ->
%          {0, merge(V1, V2)};
%        {1, V2} ->
%          {0, {1, V2}}
%      end;
%    {1, V1} ->
%      case T2 of
%        {0, V2} ->
%          {1, {0, V2}};
%        {1, V2} ->
%          {1, merge(V1, V2)}
%      end
%  end.
%
%% foldr(Func, Acc, Trie)
%foldr(Func, Acc, T) ->
%  case T of
%    {0, V1} ->
%      foldr(Func, Func(V1, Acc), V1);
%    {1, V2} ->
%      foldr(Func, Func(V2, Acc), V2)
%  end.
%
%% foldl(Func, Acc, Trie)
%foldl(Func, Acc, T) ->
%  case T of
%    {0, V1} ->
%      foldl(Func, Func(Acc, V1), V1);
%    {1, V2} ->
%      foldl(Func, Func(Acc, V2), V2)
%  end.
%
%% filter(Pred, Trie)
%filter(Pred, T) ->
%  case T of
%    {0, V1} ->
%      {0, filter(Pred, V1)};
%    {1, V2} ->
%      {1, filter(Pred, V2)}
%  end.
%
%% map(Func, Trie)
%map(Func, T) ->
%  case T of
%    {0, V1} ->
%      {0, map(Func, V1)};
%    {1, V2} ->
%      {1, map(Func, V2)}
%  end.

new_trie() -> #node{}.

trie_append([], V, Node) -> #node{value = V, children = Node#node.children};
trie_append([H | T], V, Node) ->
  TrieDict_Children = Node#node.children,
  case dict:find(H, TrieDict_Children) of
    {ok, Leaf} ->
      Leaf1 = trie_append(T, V, Leaf),
      #node{children = dict:store(H, Leaf1, TrieDict_Children)};
    error ->
      Leaf = trie_append(T, V, new_trie()),
      #node{children = dict:store(H, Leaf, TrieDict_Children)}
  end.

trie_erase([], Node) ->
  #node{value = undefined, children = Node#node.children};
trie_erase([H | T], Node) ->
  TrieDict_Children = Node#node.children,
  case dict:find(H, TrieDict_Children) of
    {ok, Leaf} ->
      Leaf1 = trie_erase(T, Leaf),
      case dict:is_empty(Leaf1#node.children) andalso Leaf1#node.value == undefined of
        true -> #node{children = dict:erase(H, TrieDict_Children)};
        false -> #node{children = dict:store(H, Leaf1, TrieDict_Children)}
      end;
    error -> Node
  end.

trie_search([], Node) -> Node#node.value;

trie_search([H | T], Node) ->
  TrieDict_Children = Node#node.children,
  case dict:find(H, TrieDict_Children) of
    {ok, Leaf} ->
      trie_search(T, Leaf);
    error ->
      undefined
  end.

trie_filter(Pred, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.children,
      NewChildren = dict:fold(
        fun(Key, Leaf, Acc) ->
          FilteredLeaf = trie_filter(Pred, Leaf),
          case dict:is_empty(FilteredLeaf#node.children) andalso FilteredLeaf#node.value == undefined of
            true -> Acc;
            false -> dict:store(Key, FilteredLeaf, Acc)
          end
        end, dict:new(), TrieDict_Children),
      #node{children = NewChildren};
    V ->
      case Pred(V) of
        true -> #node{value = V, children = dict:new()};
        false -> #node{}
      end
  end.

trie_map(Func, Leaf) -> map(Func, Leaf, new_trie()).

map(Func, Node, NewNode) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.children,
      NewChildren = dict:map(fun(_, Leaf) -> map(Func, Leaf, new_trie()) end, TrieDict_Children),
      NewNode#node{children = NewChildren};
    V ->
      NewV = Func(V),
      NewNode#node{value = NewV}
  end.

trie_foldl(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.children,
      dict:fold(fun(_, Leaf, Acc1) -> trie_foldl(Fun, Acc1, Leaf) end, Acc, TrieDict_Children);
    V -> Fun(V, Acc)
  end.

trie_foldr(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.children,
      dict:fold(fun(_, Leaf, Acc1) -> trie_foldr(Fun, Acc1, Leaf) end, Acc, TrieDict_Children);
    V -> Fun(V, Acc)
  end.

trie_merge(Tree1, Tree2) ->
  #node{value = V, children = TrieDict_Children} = Tree1,
  #node{value = _V2, children = TrieDict_Children2} = Tree2,
  case V of
    undefined ->
      NewChildren = dict:fold(
        fun(Key, Leaf2, Acc) ->
          case dict:find(Key, TrieDict_Children) of
            {ok, Leaf1} -> dict:store(Key, trie_merge(Leaf1, Leaf2), Acc);
            error -> dict:store(Key, Leaf2, Acc)
          end
        end, TrieDict_Children, TrieDict_Children2),
      #node{value = undefined, children = NewChildren};
    _ -> Tree1
  end.
