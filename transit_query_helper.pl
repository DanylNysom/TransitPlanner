% Prolog
stops_at_helper(TripName, StopId) :-
  stop_time(TripId, stop_id, StopId),
  trip(RouteId, trip_id, TripId),
  trip(RouteId, trip_headsign, TripName).

stops_at(TripName, StopId) :-
  setof(TName, stops_at_helper(TName, StopId), Trips),
  %  setof(TName, stop_time(TId, stop_id, StopId), Trips),
  !,  % I don't know if this is necessary, but there's no
      % need to look for the set of routes again
  member(TripName, Trips).

goes_between(StopCodeA, StopCodeB, RouteNum) :-
  stops_at(RouteNum, _, StopCodeA, _),
  stops_at(RouteNum, _, StopCodeB, _).
