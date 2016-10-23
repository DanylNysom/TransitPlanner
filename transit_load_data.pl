% Prolog

%----------------------------------------------%
% from http://stackoverflow.com/a/23183753
:- use_module(library(apply)).
:- use_module(library(csv)).

get_rows_data(File, Lists) :-
  csv_read_file(File, Rows, []),
  rows_to_lists(Rows, Lists).

rows_to_lists(Rows, Lists) :-
  maplist(row_to_list, Rows, Lists).

row_to_list(Row, List) :-
  Row =.. [row|List].
% END from http://stackoverflow.com/a/23183753
%----------------------------------------------%

%----------------------------------------------%
% create knowledge base
assert_with(Term, Id, Label, Value) :-
  Pred =.. [Term|[Id, Label, Value]],
  assert(Pred).

assert_row(_, _, [], []).
assert_row(Term, Id, [Label|Labels], [Value|Values]) :-
  assert_with(Term, Id, Label, Value),
  assert_row(Term, Id, Labels, Values).

assert_rows(_, _, []).
assert_rows(Term, Labels, [Values|List]) :-
  [Id|_] = Values, % Id is the first element of the row
  assert_row(Term, Id, Labels, Values),
  assert_rows(Term, Labels, List).

assert_file(Type, File) :-
  string_concat("data/", File, FilePath),
  string_concat("Loading ", FilePath, Message),
  writeln(Message),
  get_rows_data(FilePath, [Labels|Values]),
  assert_rows(Type, Labels, Values).

% separate to save the stack from overflowing
:- assert_file(agency, "agency.txt").
:- assert_file(calendar, "calendar.txt").
:- assert_file(calendar_date, "calendar_dates.txt").
:- assert_file(route, "routes.txt").
:- assert_file(shape, "shapesaa.txt").
:- assert_file(shape, "shapesab.txt").
:- assert_file(stop_time, "stop_timesaa.txt").
:- assert_file(stop_time, "stop_timesab.txt").
:- assert_file(stop_time, "stop_timesac.txt").
:- assert_file(stop_time, "stop_timesad.txt").
:- assert_file(stop_time, "stop_timesae.txt").
:- assert_file(stop_time, "stop_timesaf.txt").
:- assert_file(stop_time, "stop_timesag.txt").
:- assert_file(stop_time, "stop_timesah.txt").
:- assert_file(stop_time, "stop_timesai.txt").
:- assert_file(stop_time, "stop_timesaj.txt").
:- assert_file(stop_time, "stop_timesak.txt").
:- assert_file(stop_time, "stop_timesal.txt").
:- assert_file(stop_time, "stop_timesam.txt").
:- assert_file(stop_time, "stop_timesan.txt").
:- assert_file(stop_time, "stop_timesao.txt").
:- assert_file(stop_time, "stop_timesap.txt").
:- assert_file(stop_time, "stop_timesaq.txt").
:- assert_file(stop_time, "stop_timesar.txt").
:- assert_file(stop_time, "stop_timesas.txt").
:- assert_file(stop, "stops.txt").
:- assert_file(transfer, "transfers.txt").
:- assert_file(trip, "trips.txt").

