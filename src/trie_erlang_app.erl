%%%-------------------------------------------------------------------
%%% @author SMash
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 07. март 2023 11:58
%%%-------------------------------------------------------------------
-module(trie_erlang_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
  trie_erlang_sup:start_link().

stop(_State) ->
  ok.
