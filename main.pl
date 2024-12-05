% Load required files
:- [facts].
:- [helper].

% Parse SQL-like natural language commands into a structured query
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

% Get specific columns from a table
evaluate_logical([command, TableInfo, Operation], Results) :-
    process_tables(TableInfo, Operation, IntermediateResults),
    format_results(IntermediateResults, Results).

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



% Write Part 2 Here
format_results([], []).
format_results([[TableName, all, Rows]|RestTables], [[TableName, Headers, Rows]|FormattedRest]) :-
    table(TableName, Headers),
    format_results(RestTables, FormattedRest).
format_results([[TableName, Columns, Rows]|RestTables], [[TableName, Columns, Rows]|FormattedRest]) :-
    format_results(RestTables, FormattedRest).

process_tables(TableInfo, [], Results) :-
    get_table_data(TableInfo, Results).
process_tables(TableInfo, [where, Conditions], Results) :-
    get_table_data(TableInfo, Tables),
    apply_conditions(Tables, Conditions, Results).
process_tables(TableInfo, [matches, Values], Results) :-
    get_table_data(TableInfo, Tables),
    apply_value_match(Tables, Values, Results).
process_tables(TableInfo, [join, TargetTable, JoinColumn], Results) :-
    apply_join(TableInfo, TargetTable, JoinColumn, Results).
process_tables(TableInfo, [matches, MatchCol, [command, [[MatchCol, TargetTable]], [where, WhereCondition]]], Results) :-
    evaluate_logical([command, [[MatchCol, TargetTable]], [where, WhereCondition]], SubResults),
    get_table_data(TableInfo, Tables),
    apply_matches_subquery(Tables, SubResults, Results).

get_table_data([], []).
get_table_data([[all, TableName]|RestTables], [[TableName, Headers, Rows]|Results]) :-
    table(TableName, Headers),
    findall(CurrentRow, row(TableName, CurrentRow), UniqueRows),
    sort(UniqueRows, Rows),
    get_table_data(RestTables, Results).
get_table_data([[Columns, TableName]|RestTables], [[TableName, ColumnList, FilteredRows]|Results]) :-
    table(TableName, TableHeaders),
    (is_list(Columns) -> ColumnList = Columns ; ColumnList = [Columns]),
    findall(SelectedValues, (
        row(TableName, FullRow),
        extract_values_from_row(FullRow, TableHeaders, ColumnList, SelectedValues)
    ), TempRows),
    sort(TempRows, FilteredRows),
    get_table_data(RestTables, Results).

apply_conditions([], _, []).
apply_conditions([[TableName, Columns, _]|RestTables], Conditions, [[TableName, FinalColumns, UniqueRows]|Results]) :-
    table(TableName, TableHeaders),
    (Columns = all -> FinalColumns = TableHeaders ; FinalColumns = Columns),
    findall(SelectedValues, (
        row(TableName, FullRow),
        evaluate_condition(TableHeaders, FullRow, Conditions),
        (Columns = all ->
            SelectedValues = FullRow
        ;
            extract_values_from_row(FullRow, TableHeaders, Columns, SelectedValues)
        )
    ), TempRows),
    sort(TempRows, UniqueRows),
    apply_conditions(RestTables, Conditions, Results).

evaluate_condition(Headers, Row, [condition, Column, Op, Value]) :-
    !,
    nth0(Index, Headers, Column),
    nth0(Index, Row, RowValue),
    compare_values(RowValue, Op, Value).

evaluate_condition(Headers, Row, [and, Cond1, Cond2]) :-
    !,
    evaluate_condition(Headers, Row, Cond1),
    evaluate_condition(Headers, Row, Cond2).

evaluate_condition(Headers, Row, [or, Cond1, Cond2]) :-
    !,
    (evaluate_condition(Headers, Row, Cond1)
    ;
    evaluate_condition(Headers, Row, Cond2)), !.

evaluate_condition(_, _, []) :- !.

extract_values_from_row(CurrentRow, Headers, ColumnList, Values) :-
    findall(Value, (
        member(ColumnName, ColumnList),
        nth0(ColumnIndex, Headers, ColumnName),
        nth0(ColumnIndex, CurrentRow, Value)
    ), Values).

compare_values(Value1, '=', Value2) :-
    (atom(Value1), atom(Value2) ->
        Value1 = Value2
    ;
    try_numeric_compare(Value1, Value2, '=') ->
        true
    ;
    try_date_compare(Value1, Value2, '=')).

compare_values(Value1, Op, Value2) :-
    (Op = '>' ; Op = '<'),
    (try_numeric_compare(Value1, Value2, Op) ->
        true
    ;
    try_date_compare(Value1, Value2, Op)).

try_numeric_compare(Value1, Value2, Operator) :-
    catch((
        (atom(Value1) -> atom_number(Value1, Num1) ; number(Value1) -> Num1 = Value1),
        (atom(Value2) -> atom_number(Value2, Num2) ; number(Value2) -> Num2 = Value2),
        numeric_compare(Num1, Num2, Operator)
    ), _, fail).

numeric_compare(Num1, Num2, '<') :- Num1 < Num2.
numeric_compare(Num1, Num2, '>') :- Num1 > Num2.
numeric_compare(Num1, Num2, '=') :- Num1 =:= Num2.

try_date_compare(Value1, Value2, Operator) :-
    catch((
        atom_string(Value1, String1),
        atom_string(Value2, String2),
        split_string(String1, "-", "", [Month1Str, Day1Str, Year1Str]),
        split_string(String2, "-", "", [Month2Str, Day2Str, Year2Str]),
        maplist(atom_number, [Month1Str, Day1Str, Year1Str], [Month1, Day1, Year1]),
        maplist(atom_number, [Month2Str, Day2Str, Year2Str], [Month2, Day2, Year2]),
        compare_dates(Year1, Month1, Day1, Year2, Month2, Day2, Operator)
    ), _, fail).

compare_dates(Y1, M1, D1, Y2, M2, D2, '>') :-
    date_value(Y1, M1, D1, V1),
    date_value(Y2, M2, D2, V2),
    V1 > V2.
compare_dates(Y1, M1, D1, Y2, M2, D2, '<') :-
    date_value(Y1, M1, D1, V1),
    date_value(Y2, M2, D2, V2),
    V1 < V2.
compare_dates(Y1, M1, D1, Y2, M2, D2, '=') :-
    date_value(Y1, M1, D1, V1),
    date_value(Y2, M2, D2, V2),
    V1 =:= V2.

date_value(Year, Month, Day, Value) :-
    Value is Year * 10000 + Month * 100 + Day.

apply_value_match([], _, []).
apply_value_match([[TableName, Columns, _]|RestTables], Values, [[TableName, FinalColumns, []]|Results]) :-
    table(TableName, TableHeaders),
    (Columns = all -> FinalColumns = TableHeaders ; FinalColumns = Columns),
    apply_value_match(RestTables, Values, Results).

apply_matches_subquery([], _, []).
apply_matches_subquery([[TableName, Columns, _]|RestTables], _, [[TableName, FinalColumns, []]|Results]) :-
    table(TableName, TableHeaders),
    (Columns = all -> FinalColumns = TableHeaders ; FinalColumns = Columns),
    apply_matches_subquery(RestTables, [], Results).

apply_join([[Columns, SourceTable]], _, _, [[SourceTable, ColumnList, []]]) :-
    (is_list(Columns) -> ColumnList = Columns ; ColumnList = [Columns]).

extract_values(TargetColumn, [[_, Headers, Rows]|_], ExtractedValues) :-
    nth0(ColumnIndex, Headers, TargetColumn),
    findall(CurrentValue, (
        member(CurrentRow, Rows),
        nth0(ColumnIndex, CurrentRow, CurrentValue)
    ), TempValues),
    sort(TempValues, ExtractedValues).