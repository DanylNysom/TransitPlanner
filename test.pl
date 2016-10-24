:- load_files(['transit_load_data.pl',
               'transit_query_helper.pl']).

test(Actual, Expect) :-
  Actual = Expect, !, writeln('Test passed.') ;
  writeln(('Test Failed:')),
  writeln(('  Expected': Expect)),
  writeln(('  Actual': Actual)).

:- consult('tests/unit.pl').


