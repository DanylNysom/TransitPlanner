% Stops

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