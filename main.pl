% Load required files
:- [facts].
:- [helper].

valid_input([get, all, from, _TableName]).
valid_input([get, _Columns, from, _TableName]).

% Parse SQL-like natural language commands into a structured query
nlp_parse(LineSplit, Query) :-
    % Parse the command using DCG (to be implemented separately)  
    valid_input(LineSplit), % Ensure input validity
    phrase(command(Query), LineSplit).
nlp_parse(LineSplit, _) :-
    \+ valid_input(LineSplit), 
    writeln('Error: Invalid command syntax'), fail.

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
% Main command structure
command([command, _, CommandOperation]) -->
    get, table_column_info(_), command_operation(CommandOperation).

% Get keyword
get --> [get].

% Table column info rules
table_column_info([[all, TableName]]) --> [all, from, TableName].
table_column_info([[[Columns], TableName]]) -->
    columns(Columns), [from, TableName].
table_column_info([[[Columns1], TableName1], [[Columns2], TableName2]]) -->
    columns(Columns1), [from, TableName1, and], columns(Columns2), [from, TableName2].

% Columns parsing
columns([Col]) --> [Col].
columns([Col | Rest]) --> [Col, and], columns(Rest).
columns([Col | Rest]) --> [Col, ',', RestCols], {flatten([RestCols], Rest)}.

% Command operation rules
command_operation([]) --> [].
command_operation([join, TableName, ColumnName]) -->
    [linking, TableName, by, their, ColumnName, .].
command_operation([join, TableName, ColumnName]) -->
    [connecting, TableName, by, their, ColumnName, .].
command_operation([matches, Values]) -->
    [such, that, its, values, are, either], values(Values).
command_operation([matches, ColumnName, InnerCommand]) -->
    [such, that], [ColumnName, matches, values, within, the, ColumnName2, in, TableName],
    [where], conditions(InnerConditions),
    {InnerCommand = [command, [[ColumnName2, TableName]], [where, InnerConditions]]}.
command_operation([where, Conditions]) -->
    [where], conditions(Conditions).
command_operation(_) -->
    [invalid, operation], { fail }.

% Values parsing
values([Val]) --> [Val, .].
values([Val | Rest]) --> [Val, or], values(Rest).
values([Val | Rest]) --> [Val, ',', RestVals, .], {flatten([RestVals], Rest)}.

% Conditions parsing
conditions([Condition]) --> condition(Condition), [.].
conditions([and, C1, C2]) --> condition(C1), [and], conditions(C2).
conditions([or, C1, C2]) --> condition(C1), [or], conditions(C2).

% Single condition parsing
condition([condition, Col, Op, Val]) -->
    [Col], equality(Op), [Val].

% Equality parsing
equality('=') --> [equals].
equality('<') --> [is, less, than].
equality('>') --> [is, greater, than].
