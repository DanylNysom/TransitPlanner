% Prolog
stops_at_helper(RouteId, StopId) :-
  stop_time(TripId, stop_id, StopId),
  trip(RouteId, trip_id, TripId).

stops_at(RouteId, StopId) :-
  setof(RId, stops_at_helper(RId, StopId), Routes),
  !,  % I don't know if this is necessary, but there's no
      % need to look for the set of routes again
  member(RouteId, Routes).

goes_between(StopCodeA, StopCodeB, RouteNum) :-
  stops_at(RouteNum, _, StopCodeA, _),
  stops_at(RouteNum, _, StopCodeB, _).
