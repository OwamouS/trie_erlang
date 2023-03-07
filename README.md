Машковцев А. Л. Р34102
Функциональное программирование. Лабораторная работа №2 - prefix tree(trie). Erlang
=====
Базовые функции префиксного дерева:
-----
Создать пустое дерево:
```
new_trie() ->
  #node{}.
```
Добавить лист
-----
```
trie_append([], V, Node) ->
  #node{value = V, children = Node#node.children};
trie_append([H | T], V, Node) ->
  TrieDict_Children = Node#node.children,
  case dict:find(H, TrieDict_Children) of
    error ->
      Leaf = trie_append(T, V, new_trie()),
      #node{children = dict:store(H, Leaf, TrieDict_Children)};
    {ok, Leaf} ->
      Leaf1 = trie_append(T, V, Leaf),
      #node{children = dict:store(H, Leaf1, TrieDict_Children)}
  end.
```
Удалить ноду
-----
```
trie_erase([], Node) ->
  #node{value = undefined, children = Node#node.children};
trie_erase([H | T], Node) ->
  TrieDict_Children = Node#node.children,
  case dict:find(H, TrieDict_Children) of
    error ->
      Node;
    {ok, Leaf} ->
      Leaf1 = trie_erase(T, Leaf),
      case dict:is_empty(Leaf1#node.children) andalso Leaf1#node.value == undefined of
        true ->
          #node{children = dict:erase(H, TrieDict_Children)};
        false ->
          #node{children = dict:store(H, Leaf1, TrieDict_Children)}
      end
  end.
```
Поиск по ключу
-----
```
trie_search([], Node) ->
  Node#node.value;
trie_search([H | T], Node) ->
  TrieDict_Children = Node#node.children,
  case dict:find(H, TrieDict_Children) of
    error ->
      undefined;
    {ok, Leaf} ->
      trie_search(T, Leaf)
  end.
```
Левая свертка
-----
```
foldl(Fun, Acc, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.children,
      dict:fold(fun(_, Leaf, Acc1) -> foldl(Fun, Acc1, Leaf) end, Acc, TrieDict_Children);
    V ->
      Fun(V, Acc)
  end.
```
Фильтр по предикате
-----
```
trie_filter(Predicate, Node) ->
  case Node#node.value of
    undefined ->
      TrieDict_Children = Node#node.children,
      NewChildren = dict:fold(
        fun(Key, Leaf, Acc) ->
          FilteredLeaf = trie_filter(Predicate, Leaf),
          case dict:is_empty(FilteredLeaf#node.children) andalso FilteredLeaf#node.value == undefined of
            true -> Acc;
            false -> dict:store(Key, FilteredLeaf, Acc)
          end
        end, dict:new(), TrieDict_Children),
      #node{children = NewChildren};
    V ->
      case Predicate(V) of
        true -> #node{value = V, children = dict:new()};
        false -> #node{}
      end
  end.
```
Объединить деревья
-----
```
trie_merge(Tree1, Tree2) ->
  #node{value = V, children = TrieDict_Children} = Tree1,
  #node{value = _Value2, children = Children2} = Tree2,
  case V of
    undefined ->
      NewChildren = dict:fold(
        fun(Key, Child2, Acc) ->
          case dict:find(Key, TrieDict_Children) of
            error ->
              dict:store(Key, Child2, Acc);
            {ok, Child1} ->
              dict:store(Key, trie_merge(Child1, Child2), Acc)
          end
        end, TrieDict_Children, Children2),
      #node{value = undefined, children = NewChildren};
    _ ->
      Tree1
  end.
```
