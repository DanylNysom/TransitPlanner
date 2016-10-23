% Prolog

% TODO Document the purpose of this file

% Type: atom
% Row: row(...)
assert_row(Type, Row) :-
  Row =.. [row|Values],
  Pred =.. [Type|Values],
  assert(Pred).

% Type: atom
% Row: row(...)
% Rest: listof row(...)
assert_rows(_, []).
assert_rows(Type, [Row|Rest]) :-
  assert_row(Type, Row),
  assert_rows(Type, Rest).

% Type/Arity: atom/integer
% Label: atom
% Index: integer
assert_column(Type / Arity, Label, Index) :-
  length(Fields, Arity),
  nth0(Index, Fields, X),
  % Label(Type(Fields), X).
  Struct =.. [Type|Fields],
  Pred =.. [Label,Struct,X],
  assert(Pred).

% Type: atom
% Header: row(...)
assert_columns(Type, Header) :-
  Header =.. [row|Labels],
  length(Labels, Arity),
  forall(nth0(Index, Labels, Label), assert_column(Type / Arity, Label, Index)).

% Type: atom
% Files: codes (string)
assert_file(Type, File) :-
  string_concat("Loading ", File, Message),
  writeln(Message),
  % Header is row(...), Body is row(...)[]
  csv_read_file(File, [Header|Body], []),
  assert_columns(Type, Header),
  assert_rows(Type, Body).

% parses data
:- assert_file(agency, "data/agency.txt").
:- assert_file(calendar, "data/calendar.txt").
:- assert_file(calendar_date, "data/calendar_dates.txt").
:- assert_file(route, "data/routes.txt").
:- assert_file(shape, "data/shapes.txt").
:- assert_file(stop_time, "data/stop_times.txt").
:- assert_file(stop, "data/stops.txt").
:- assert_file(transfer, "data/transfers.txt").
:- assert_file(trip, "data/trips.txt").

