% Prolog
stops_at(Trip, Stop) :-
  % stop_id of stop
  stop_id(Stop, StopId),
  setof(
    Trip0,
    setof(
    (StopTime, TripId), % Un-interesting free variables
    (
      % stop_id of stop_time
      stop_id(StopTime, StopId),
      trip_id(StopTime, TripId),
      StopTime,
      trip_id(Trip0, TripId),
      Trip0 = trip(_,_,_,_,_,_,_,_),
      Trip0
    ),
    _
    ),
    Trips
  ),
  member(Trip, Trips).

goes_between(StopA, StopB, Trip) :-
  stops_at(Trip, StopA),
  stops_at(Trip, StopB).

% stop_times
% (StartStopId, StartTime): stop_id, time(string)
% (DestStopId, DestTime): stop_id, time(string)
% TripId: Out trip_id
directly_reachable((StartStopId, StartTime), (DestStopId, DestTime), TripId) :- true,
  time_second(StartTime, StartSecond),
  time_second(DestTime, DestSecond),
  % departure_time is later than StartTime
  stop_id(StartStopTime, StartStopId),
  StartStopTime,
  departure_time(StartStopTime, DepartureTime),
  time_second(DepartureTime, DepartureSecond),
  StartSecond < DepartureSecond,
  % shares the same trip
  trip_id(StartStopTime, TripId),
  trip_id(DestStopTime, TripId),
  % arrival_time is earlier than DestTime
  stop_id(DestStopTime, DestStopId),
  DestStopTime,
  arrival_time(DestStopTime, ArrivalTime),
  time_second(ArrivalTime, ArrivalSecond),
  ArrivalSecond < DestSecond.
  

% stops
% (Lat, Lon): (number, number)
% Stop: Out stop(...)
nearest_stop((Lat,Lon), Stop) :-
  findall(
    Dis0 - Stop0,
    (
      stop_lat(Stop0, Lat0),
      stop_lon(Stop0, Lon0),
      Stop0,
      latlon_distance((Lat0, Lon0), (Lat, Lon), Dis0)
    ),
    DisStopMap),
  keysort(DisStopMap, [_ - Stop|_]).


% Utils
% (Lat0, Lon0): (number, number)
% (Lat1, Lon1): (number, number)
% Distance: Out number
latlon_distance((Lat0,Lon0), (Lat1, Lon1), Distance) :-
  Distance is sqrt((Lat0 - Lat1) ^ 2 + (Lon0 - Lon1) ^ 2).

% Time: string {Hour}:{Minute}:{Second} % \s will be ignored
% Second: Out integer
time_second(Time, Second) :-
  split_string(Time, ':', ' ', [Hstr, Mstr, Sstr]),
  atom_number(Hstr, H),
  atom_number(Mstr, M),
  atom_number(Sstr, S),
  Second is H * 3600 + M * 60 + S.