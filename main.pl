% Load required files
:- [facts].
:- [helper].

% Parse SQL-like natural language commands into a structured query
nlp_parse(LineSplit, Query) :-
    % Parse the command using DCG (to be implemented separately)
    print(LineSplit).

% Evaluate the parsed logical query and return the filtered table
evaluate_logical([command, TableColumnInfo, Conditions], FilteredTable) :-
    % Process the table information
    process_table_info(TableColumnInfo, TablesData),
    % Apply conditions for filtering rows
    process_conditions(TablesData, Conditions, FilteredTable).

% Parse individual commands and evaluate
parse_and_evaluate(_, [], []).

parse_and_evaluate(part1, [[_, LineSplit] | T], [Query | ResultTail]) :-
    nlp_parse(LineSplit, Query),
    write(Query),nl, % Print the parsed query for part 1
    parse_and_evaluate(part1, T, ResultTail).

parse_and_evaluate(part2, [[Line, LineSplit] | T], [Result | ResultTail]) :-
    writeln(Line), % Print the original command line for part 2
    nlp_parse(LineSplit, Query),
    evaluate_logical(Query, FilteredTable),
    print_tables(FilteredTable), % Use helper.pl to print the tables
    parse_and_evaluate(part2, T, ResultTail).

% Main entry point for the program
main :-
    current_prolog_flag(argv, [DataFile, PrintOption | _]),
    open(DataFile, read, Stream),
    read_file(Stream, Lines), % Read the input file into structured lines
    close(Stream),
    parse_and_evaluate(PrintOption, Lines, _).

% Helper functions for table and row processing
process_table_info([[all, TableName]], [[TableName, Headers, Rows]]) :-
    table(TableName, Headers),
    findall(Row, row(TableName, Row), Rows).

process_table_info([[[Columns], TableName]], [[TableName, Columns, Rows]]) :-
    table(TableName, AllHeaders),
    find_indices(AllHeaders, Columns, Indices),
    findall(FilteredRow, (row(TableName, Row), select_indices(Row, Indices, FilteredRow)), Rows).

% Apply conditions to filter rows
process_conditions(TablesData, [], TablesData). % No conditions, return all rows
process_conditions([[TableName, Headers, Rows]], [where, Conditions], [[TableName, Headers, FilteredRows]]) :-
    include(matches_conditions(Conditions, Headers), Rows, FilteredRows).

% Utility predicates
find_indices(AllHeaders, Columns, Indices) :-
    findall(Index, (nth1(Index, AllHeaders, Col), member(Col, Columns)), Indices).

select_indices(Row, Indices, FilteredRow) :-
    findall(Value, (nth1(Index, Row, Value), member(Index, Indices)), FilteredRow).

matches_conditions([], _Headers, _Row) :- true.
matches_conditions([[condition, Col, Op, Value] | Rest], Headers, Row) :-
    nth1(Index, Headers, Col),
    nth1(Index, Row, CellValue),
    satisfies_condition(CellValue, Op, Value),
    matches_conditions(Rest, Headers, Row).

% Check if a cell satisfies a condition
satisfies_condition(CellValue, '=', Value) :- CellValue = Value.
satisfies_condition(CellValue, '<', Value) :- CellValue @< Value.
satisfies_condition(CellValue, '>', Value) :- CellValue @> Value.

% DCG rules for command parsing (to be fully implemented)
command([command, TableColumnInfo, Conditions]) -->
    get, table_column_info(TableColumnInfo), command_operation(Conditions).

get --> [get].
table_column_info([[all, TableName]]) --> [all, from, TableName].
table_column_info([[[Columns], TableName]]) -->
    columns(Columns), [from, TableName].
columns([Col]) --> [Col].
columns([Col | Rest]) --> [Col, and], columns(Rest).
command_operation([]) --> [].
command_operation([where, Conditions]) --> [where], conditions(Conditions).

conditions([Condition]) --> condition(Condition).
conditions([Condition, and | Rest]) --> condition(Condition), [and], conditions(Rest).
condition([condition, Col, Op, Value]) -->
    [Col], equality(Op), [Value].

equality('=') --> [equals].
equality('<') --> [is, less, than].
equality('>') --> [is, greater, than].
