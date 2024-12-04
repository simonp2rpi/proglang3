% Load required files
:- [facts].
:- [helper].

% Parse SQL-like natural language commands into a structured query
nlp_parse(LineSplit, Query) :-
    % Parse the command using DCG
    command(LineSplit)
    debug_msg("Parsed query", Query).

evaluate_logical([command, TableColumnInfo, Conditions], FilteredTable) :-
    % Process the table information
    process_table_info(TableColumnInfo, TablesData),
    % Apply conditions for filtering rows
    process_conditions(TablesData, Conditions, FilteredTable).

% Parse individual commands and evaluate
parse_and_evaluate(_,[], []).

parse_and_evaluate(part1,[[_,LineSplit]|T], [Query|ResultTail]):- 
    nlp_parse(LineSplit,Query),
    write(Query),nl,
    parse_and_evaluate(part1,T,ResultTail).
                
parse_and_evaluate(part2,[[Line,LineSplit]|T], [Result|ResultTail]):- 
    write(Line),nl,
    nlp_parse(LineSplit,Query),
    evaluate_logical(Query,FilteredTable),
    %write("\t"),write(FilteredTable),nl,
    print_tables(FilteredTable),
    parse_and_evaluate(part2,T,ResultTail).
% Main 
main :-
    current_prolog_flag(argv, [DataFile, PrintOption|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain individual line within the file split by spaces and special character like (,) and (.) . 
    close(Stream),
	parse_and_evaluate(PrintOption,Lines,_).

% Main command structure
command([command, TableColumnInfo, CommandOperation]) -->
    get, table_column_info(TableColumnInfo), command_operation(CommandOperation).

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
    [linking, TableName, by, their, ColumnName].
command_operation([matches, Values]) -->
    [such, that, its, values, are, either], values(Values).
command_operation([matches, ColumnName, InnerCommand]) -->
    [such, that], [ColumnName, matches, values, within, the, ColumnName2, in, TableName],
    [where], conditions(InnerConditions),
    {InnerCommand = [command, [[ColumnName2, TableName]], [where, InnerConditions]]}.
command_operation([where, Conditions]) -->
    [where], conditions(Conditions).

% Values parsing
values([Val]) --> [Val].
values([Val | Rest]) --> [Val, or], values(Rest).
values([Val | Rest]) --> [Val, ',', RestVals], {flatten([RestVals], Rest)}.

% Conditions parsing
conditions([Condition]) --> condition(Condition).
conditions([and, C1, C2]) --> conditions(C1), [and], conditions(C2).
conditions([or, C1, C2]) --> condition(C1), [or], condition(C2).

% Single condition parsing
condition([condition, Col, Op, Val]) -->
    [Col], equality(Op), [Val].

% Equality parsing
equality('=') --> [equals].
equality('<') --> [is, less, than].
equality('>') --> [is, greater, than].

% Debugging utility
debug_msg(Msg, Data) :- write(Msg), write(": "), writeln(Data).