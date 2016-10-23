% Prolog

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
  parse_specifiers(List, Specifiers),
  setof(Bus, interpret_specifiers(Specifiers, route, Bus), Answer).

parse_which_query([bus|List], Answer) :-
  parse_specifiers(List, Specifiers),
  interpret_specifiers(Specifiers, route, Answer).

parse_specifiers([], _).

parse_specifiers([stop,at|List], [Specifier|MoreSpecifiers]) :-
  parse_specifiers([stops,at|List], [Specifier|MoreSpecifiers]).

parse_specifiers([stops,at|List], [Specifier|MoreSpecifiers]) :-
  parse_specifiers_stopsAt(List, Rest, Specifier),
  parse_specifiers(Rest, MoreSpecifiers).

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
  stops_at(RouteId, StopId),
  route(RouteId, route_long_name, Answer).
