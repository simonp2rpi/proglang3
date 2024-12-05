% Load required files
:- [facts].
:- [helper].

% Parse SQL-like natural language commands into a structured query
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

% Get specific columns from a table
evaluate_logical(Query,FilteredTable) :-
    Query = [command, TableColumnInfo, Conditions],
    % writeln(TableColumnInfo),
    % writeln(Conditions),
    TableColumnInfo = [[Columns, Table]],
    % writeln(Columns),
    % writeln(Table),
    phrase(tableCommand(FilteredTable), Query),
    writeln(FilteredTable).


% Parse individual commands and evaluate
parse_and_evaluate(_,[], []).

parse_and_evaluate(part1,[[_,LineSplit]|T], [Query|ResultTail]):- 
    nlp_parse(LineSplit,Query),
    writeln(Query),
    parse_and_evaluate(part1,T,ResultTail).
                
parse_and_evaluate(part2,[[Line,LineSplit]|T], [Result|ResultTail]):- 
    write(Line),nl,
    nlp_parse(LineSplit,Query),
    evaluate_logical(Query,FilteredTable),
    write("\t"),write(FilteredTable),nl,
    % print_tables(FilteredTable),
    parse_and_evaluate(part2,T,ResultTail).

% Main 
main :-
    current_prolog_flag(argv, [DataFile, PrintOption|_]),
    open(DataFile, read, Stream),
    read_file(Stream,Lines), %Lines contain individual line within the file split by spaces and special character like (,) and (.) . 
    close(Stream),
	parse_and_evaluate(PrintOption,Lines,_).


% DCG Grammar
command([command, TableColumnInfo, CommandOperation]) --> 
    [Get], table_column_info(TableColumnInfo), command_operation(CommandOperation).
    
table_column_info([[all, Table]]) -->  [all, from], table(Table).
table_column_info([[Columns, Table]]) --> columns(Columns), [from], table(Table).
table_column_info([TableColumnDetail | Rest]) --> table_column_detail(TableColumnDetail), [and], table_column_info(Rest).
    
table_column_detail([all, Table]) --> [all, from], table(Table).
table_column_detail([Columns, Table]) --> columns(Columns), [from], table(Table).
    
command_operation([]) --> [.].
command_operation(JoinOperation) --> join_operation(JoinOperation), [.].
command_operation(MatchOperation) --> match_operation(MatchOperation), [.].
command_operation(WhereOperation) --> where_operation(WhereOperation), [.].


join_operation([join, Table, Col]) --> [linking], table(Table), [by, their], col(Col).
join_operation([join, Table, Col]) --> [connecting], table(Table), [by, their], col(Col).

match_operation([matches, Values]) --> [such, that, its, values, are, either], values(Values).
match_operation([matches, Col, [command, [[Col2, Table2]], WhereOperation]]) --> 
[such, that], col(Col), [matches, values, within, the], col(Col2), [in], table(Table2), where_operation(WhereOperation).
    
where_operation([where, OrConditions]) --> [where], or_condition(OrConditions).
where_operation([where, [and | OrConditions]]) --> [where], or_condition(FirstCondition), [and], or_condition(SecondCondition),
{append([FirstCondition], [SecondCondition], OrConditions)}.
    
% Case for 'and' with nested 'and' conditions
where_operation([where, [and | OrConditions]]) --> 
    [where], 
    or_condition(FirstCondition), 
    [and], 
    nested_and_conditions(NestedConditions),
    {append([FirstCondition], [NestedConditions], OrConditions)}.

% Parsing nested 'and' conditions
nested_and_conditions([and | OrConditions]) -->
    or_condition(FirstCondition), 
    [and], 
    or_condition(SecondCondition),
    {append([FirstCondition], [SecondCondition], OrConditions)}.

nested_and_conditions([and | OrConditions]) -->
    or_condition(FirstCondition), 
    [and], 
    nested_and_conditions(NestedConditions),
    {append([FirstCondition], [NestedConditions], OrConditions)}.

or_condition([condition, Col, Equality, Val]) --> condition(Col, Equality, Val).
or_condition([or, Condition1, Condition2]) --> [either], condition(Col1, Equality1, Val1), [or], condition(Col2, Equality2, Val2), 
{Condition1 = [condition, Col1, Equality1, Val1], Condition2 = [condition, Col2, Equality2, Val2]}.
    
condition(Col, Equality, Val) --> col(Col), equality(Equality), val(Val).
    
equality(<) --> [is, less, than].
equality(>) --> [is, greater, than].
equality(=) --> [equals].
    
table(Table) --> [Table], {atom(Table)}.
columns([Col]) --> col(Col).
columns([Col | Rest]) --> col(Col), [','], columns(Rest).
columns([Col1, Col2]) --> col(Col1), [and], col(Col2).
columns([Col | Rest]) --> col(Col), [and], columns(Rest).
columns([Col1, Col2]) --> col(Col1), [','], col(Col2).
    
col(Col) --> [Col], {atom(Col)}.
values([Val]) --> val(Val).
values([Val | Rest]) --> val(Val), [','], values(Rest).
values([Val1, Val2]) --> val(Val1), [or], val(Val2).
values([Val | Rest]) --> val(Val), [or], values(Rest).
values([Val1, Val2]) --> val(Val1), [','], val(Val2).
    
val(Val) --> [Val], {atom(Val)}.

% Part 2
tableCommand([command, TableColumnInfo, Conditions]) --> 
    tableName(TableColumnInfo), tableColumnHeader(TableColumnInfo)/*, filteredRows(Conditions)*/.

tableName([[_, Table]]) --> [Table], { table(Table, _) }.
/*
tableColumnHeader([[_, Table]]) --> 
    { table(Table, Columns) }, ( 
        [all] -> { true } ; ColumnsRequested = [[Column|Rest]],
        { validate_columns(ColumnsRequested, Columns) }
    ).
*/
% suggested alternative
tableColumnHeader([[all, Table]]) -->
    [all],
    { table(Table, _) }.
    
tableColumnHeader([[Columns, Table]]) -->
    columns(Columns),
    { table(Table, AvailableColumns),
      validate_columns(Columns, AvailableColumns) }.

% Function to ensure the column called is valid
validate_columns([], _).
validate_columns([Column|Rest], AvailableColumns) :-
    member(Column, AvailableColumns),
    validate_columns(Rest, AvailableColumns).

filteredRows([]) --> []. % No filtering: return all rows.
filteredRows([join, Table1, Column]) --> { findall(_, fail, []) }. % Join is always empty
filteredRows([matches, Values]) --> { findall(_, fail, []) }. % Match is always empty
filteredRows([where, Condition]) --> filter_condition(Condition).

filter_condition([condition, Column, Operator, Value]) --> { 
    % Fetch rows from the table and apply the condition.
    table(Table, Columns),
    nth1(Index, Columns, Column), % Find the index of the column in the table.
    findall(Row, (row(Table, Row), evaluate_condition(Row, Index, Operator, Value)), FilteredRows)
    },
    [FilteredRows].

filter_condition([and, Cond1, Cond2]) --> 
    filter_condition(Cond1), 
    filter_condition(Cond2).

filter_condition([or, Cond1, Cond2]) --> { 
    filter_condition(Cond1, Filtered1, []),
    filter_condition(Cond2, Filtered2, []),
    append(Filtered1, Filtered2, FilteredRows)
    },
    [FilteredRows].

evaluate_condition(Row, Index, =, Value) :- nth1(Index, Row, RowValue), RowValue = Value.
evaluate_condition(Row, Index, <, Value) :- nth1(Index, Row, RowValue), RowValue @< Value.
evalute_condition(Row, Index, >, Value) :- nth1(Index, Row, RowValue), RowValue @> Value.
