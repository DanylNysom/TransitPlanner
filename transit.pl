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
assert_agency(_,[],[]).
assert_agency(Id, [Label|Labels], [Value|Values]) :-
  asserta(agency(Id, Label, Value)),
  assert_agency(Id, Labels, Values).

assert_agencies(_, []).
assert_agencies(Labels, [[Id|Values]|List]) :-
  assert_agency(Id,Labels,Values),
  assert_agencies(Labels, List).

assert_calendar(_,[],[]).
assert_calendar(Id, [Label|Labels], [Value|Values]) :-
  asserta(calendar(Id, Label, Value)),
  assert_calendar(Id, Labels, Values).

assert_calendars(_, []).
assert_calendars(Labels, [[Id|Values]|List]) :-
  assert_calendar(Id,Labels,Values),
  assert_calendars(Labels, List).

assert_calendar_date(_,[],[]).
assert_calendar_date(Id, [Label|Labels], [Value|Values]) :-
  asserta(calendar_date(Id, Label, Value)),
  assert_calendar_date(Id, Labels, Values).

assert_calendar_dates(_, []).
assert_calendar_dates(Labels, [[Id|Values]|List]) :-
  assert_calendar_date(Id,Labels,Values),
  assert_calendar_dates(Labels, List).

assert_route(_,[],[]).
assert_route(Id, [Label|Labels], [Value|Values]) :-
  asserta(route(Id, Label, Value)),
  assert_route(Id, Labels, Values).

assert_routes(_, []).
assert_routes(Labels, [[Id|Values]|List]) :-
  assert_route(Id,Labels,Values),
  assert_routes(Labels, List).

assert_shape(_,[],[]).
assert_shape(Id, [Label|Labels], [Value|Values]) :-
  asserta(shape(Id, Label, Value)),
  assert_shape(Id, Labels, Values).

assert_shapes(_, []).
assert_shapes(Labels, [[Id|Values]|List]) :-
  assert_shape(Id,Labels,Values),
  assert_shapes(Labels, List).

assert_stop_time(_,[],[]).
assert_stop_time(Id, [Label|Labels], [Value|Values]) :-
  asserta(stop_time(Id, Label, Value)),
  assert_stop_time(Id, Labels, Values).

assert_stop_times(_, []).
assert_stop_times(Labels, [[Id|Values]|List]) :-
  assert_stop_time(Id,Labels,Values),
  assert_stop_times(Labels, List).

assert_stop(_,[],[]).
assert_stop(Id, [Label|Labels], [Value|Values]) :-
  asserta(stop(Id, Label, Value)),
  assert_stop(Id, Labels, Values).

assert_stops(_, []).
assert_stops(Labels, [[Id|Values]|List]) :-
  assert_stop(Id,Labels,Values),
  assert_stops(Labels, List).

assert_transfer(_,[],[]).
assert_transfer(Id, [Label|Labels], [Value|Values]) :-
  asserta(transfer(Id, Label, Value)),
  assert_transfer(Id, Labels, Values).

assert_transfers(_, []).
assert_transfers(Labels, [[Id|Values]|List]) :-
  assert_transfer(Id,Labels,Values),
  assert_transfers(Labels, List).

assert_trip(_,[],[]).
assert_trip(Id, [Label|Labels], [Value|Values]) :-
  asserta(trip(Id, Label, Value)),
  assert_trip(Id, Labels, Values).

assert_trips(_, []).
assert_trips(Labels, [[Id|Values]|List]) :-
  assert_trip(Id,Labels,Values),
  assert_trips(Labels, List).

assert_all("agency", Labels, List) :-
  assert_agencies(Labels, List).
assert_all("calendar", Labels, List) :-
  assert_calendars(Labels, List).
assert_all("calendar_date", Labels, List) :-
  assert_calendar_dates(Labels, List).
assert_all("route", Labels, List) :-
  assert_routes(Labels, List).
assert_all("shape", Labels, List) :-
  assert_shapes(Labels, List).
assert_all("stop_time", Labels, List) :-
  assert_stop_times(Labels, List).
assert_all("stop", Labels, List) :-
  assert_stops(Labels, List).
assert_all("transfer", Labels, List) :-
  assert_transfers(Labels, List).
assert_all("trip", Labels, List) :-
  assert_trips(Labels, List).

assert_file(Type, File) :-
  string_concat("Loading ", File, Message),
  writeln(Message),
  get_rows_data(File, [[_|Labels]|List]),
  assert_all(Type, Labels, List).

% separate to save the stack from overflowing
:- assert_file("agency", "agency.txt").
:- assert_file("calendar", "calendar.txt").
:- assert_file("calendar_date", "calendar_dates.txt").
:- assert_file("route", "routes.txt").
:- assert_file("shape", "shapesaa.txt").
:- assert_file("shape", "shapesab.txt").
:- assert_file("stop_time", "stop_timesaa.txt").
:- assert_file("stop_time", "stop_timesab.txt").
:- assert_file("stop_time", "stop_timesac.txt").
:- assert_file("stop_time", "stop_timesad.txt").
:- assert_file("stop_time", "stop_timesae.txt").
:- assert_file("stop_time", "stop_timesaf.txt").
:- assert_file("stop_time", "stop_timesag.txt").
:- assert_file("stop_time", "stop_timesah.txt").
:- assert_file("stop_time", "stop_timesai.txt").
:- assert_file("stop_time", "stop_timesaj.txt").
:- assert_file("stop_time", "stop_timesak.txt").
:- assert_file("stop_time", "stop_timesal.txt").
:- assert_file("stop_time", "stop_timesam.txt").
:- assert_file("stop_time", "stop_timesan.txt").
:- assert_file("stop_time", "stop_timesao.txt").
:- assert_file("stop_time", "stop_timesap.txt").
:- assert_file("stop_time", "stop_timesaq.txt").
:- assert_file("stop_time", "stop_timesar.txt").
:- assert_file("stop_time", "stop_timesas.txt").
:- assert_file("stop", "stops.txt").
:- assert_file("transfer", "transfers.txt").
:- assert_file("trip", "trips.txt").
% END creating knowledge base
%----------------------------------------------%

stops_at_helper(RouteId, StopCode) :-
  stop(StopId, 'stop_code', StopCode),
  stop_time(TripId, 'stop_id', StopId),
  trip(RouteId, 'trip_id', TripId).

stops_at(RouteNum, RouteName, StopCode, StopName) :-
  setof(RouteId, stops_at_helper(RouteId, StopCode), Trips),
  member(RouteId,Trips),
  route(RouteId, 'route_short_name', RouteNum),
  route(RouteId, 'route_long_name', RouteName),
  stop(StopId,'stop_code', StopCode),
  stop(StopId,'stop_name', StopName).

