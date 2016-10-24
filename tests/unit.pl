% Tests
:- time_second(' 8:20:00', X), test(X, 30000).
:- latlon_distance((0,0),(1,1),X),
   compare(L, X, 1.42), test(L, <),
   compare(G, X, 1.41), test(G, >).

:- nearest_stop((0,0), Stop),
   stop_id(Stop, StopId),
   test(StopId, 9956).

:- setof(
     X,
     directly_reachable(
       (56, '8:21:00'),
       (58, '8:25:00'),
       X
     ),
     Xs
   ),
   test(Xs, [8198574, 8199777]).