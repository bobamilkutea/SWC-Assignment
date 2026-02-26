:- use_module(library(http/json)).
test :-
    open('test.json', write, S),
    write(S, '{"list": ["A", "B"]}'),
    close(S),
    open('test.json', read, S2),
    json_read_dict(S2, Dict),
    close(S2),
    writeln(Dict.list).
