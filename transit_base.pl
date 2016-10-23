% Prolog

:- set_prolog_stack(global, limit(1 000 000 000)).
:- set_prolog_stack(local, limit(1 000 000 000)).

% load data
:- [transit_load_data].

% low-level helpers for queries
:- [transit_query_helper].

query(Query, Answer) :-
  atomic_list_concat(List, ' ', Query),
  parse_query(List, Answer).

parse_query([which|List], Answer) :-
  parse_which_query(List, Answer).

parse_which_query([buses|List], Answer) :-
  parse_specifiers(List, _, Specifiers),
  !,
  setof(Bus, interpret_specifiers(Specifiers, route, Bus), Answer), !.

parse_which_query([bus|List], Answer) :-
  parse_specifiers(List, _, Specifiers),
  !,
  interpret_specifiers(Specifiers, route, Answer).

parse_specifiers([], _, _) :- !.
parse_specifiers([stop,at|List], _, [Specifier|MoreSpecifiers]) :-
  parse_specifiers([stops,at|List], [stops,at], [Specifier|MoreSpecifiers]).

parse_specifiers([stops,at|List], _, [Specifier|MoreSpecifiers]) :-
  parse_specifiers_stopsAt(List, Rest, Specifier),
  parse_specifiers(Rest, [stops,at], MoreSpecifiers).

parse_specifiers([and|List], Previous, Specifiers) :-
  parse_specifiers(List, Previous, Specifiers).

parse_specifiers([stops,at|List], [stops,at], [Specifier|MoreSpecifiers]) :-
  parse_specifiers_stopsAt(List, Rest, Specifier),
  parse_specifiers(Rest, [stops,at], MoreSpecifiers).

parse_specifiers([and|List], Previous, Specifiers) :-
  List = [_|_], %make sure not empty
  Previous = [_|_],
  append(Previous,List,NewList),
  parse_specifiers(NewList, [], Specifiers).

parse_specifiers(List, Previous, Specifiers) :-
  List = [_|_], %make sure not empty
  Previous = [_|_],
  append(Previous,List,NewList),
  parse_specifiers(NewList, [], Specifiers).

parse_specifiers_stopsAt([stops|List], Rest, Specifiers) :-
  parse_specifiers_stopsAt(List, Rest, Specifiers). 

parse_specifiers_stopsAt([stop|List], Rest, Specifier) :-
  parse_specifiers_stopsAt(List, Rest, Specifier). 

parse_specifiers_stopsAt([StopCodeAtom|List], List, [stop_code, StopCode]) :-
  atom_number(StopCodeAtom, StopCode).

interpret_specifiers([], _, _).

interpret_specifiers([[Key,Value]|Specifiers], Type, Answer) :-
  interpret_specifier(Key, Value, Type, Answer),
  interpret_specifiers(Specifiers, Type, Answer).

interpret_specifier(stop_code, StopCode, route, Answer) :-
  stop(StopId, stop_code, StopCode),
  !,  % cut here because otherwise Prolog looks for all of the
      % other stops that the Answer route it just found stops at,
      % and returns the same answer for each of them
  stops_at(Answer, StopId).

