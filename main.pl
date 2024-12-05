% Load required files
:- [facts].
:- [helper].

% Parse SQL-like natural language commands into a structured query
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

% Get specific columns from a table
evaluate_logical([command, TableColumnInfo, Conditions], FilteredTable) :-
    table_command(TableColumnInfo, Conditions, Results),
    table_output(Results, FilteredTable).

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

% Making special cases for whether all or a specific Column is used, as further steps in the DCG need to account for this.
table_column_info([[all, Table]]) -->  [all, from], table(Table).
table_column_info([[Columns, Table]]) --> columns(Columns), [from], table(Table).
table_column_info([TableColumnDetail | Rest]) --> table_column_detail(TableColumnDetail), [and], table_column_info(Rest).
    
table_column_detail([all, Table]) --> [all, from], table(Table).
table_column_detail([Columns, Table]) --> columns(Columns), [from], table(Table).

% The main command_operation, seperated into three different parts.
% Join, Match, and Where.
command_operation([]) --> [.].
command_operation(JoinOperation) --> join_operation(JoinOperation), [.].
command_operation(MatchOperation) --> match_operation(MatchOperation), [.].
command_operation(WhereOperation) --> where_operation(WhereOperation), [.].

% Join operation joins a table with a column and is indicated by words like linking and connecting
join_operation([join, Table, Col]) --> [linking], table(Table), [by, their], col(Col).
join_operation([join, Table, Col]) --> [connecting], table(Table), [by, their], col(Col).

% Match operations matches up values in a column to give specific rows
match_operation([matches, Values]) --> [such, that, its, values, are, either], values(Values).
match_operation([matches, Col, [command, [[Col2, Table2]], WhereOperation]]) --> 
[such, that], col(Col), [matches, values, within, the], col(Col2), [in], table(Table2), where_operation(WhereOperation).

% Where operations are conditionals, that use or conditions to do logic 
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

% The or conditions mentioned in the where_operation, which are responsible for logic
or_condition([condition, Col, Equality, Val]) --> condition(Col, Equality, Val).
or_condition([or, Condition1, Condition2]) --> [either], condition(Col1, Equality1, Val1), [or], condition(Col2, Equality2, Val2), 
{Condition1 = [condition, Col1, Equality1, Val1], Condition2 = [condition, Col2, Equality2, Val2]}.
    
condition(Col, Equality, Val) --> col(Col), equality(Equality), val(Val).
    
% The many basic building blocks of the DCG, using basic logic and queries
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
table_output([], []).
table_output([[TableName, all, Rows]|RestTables], [[TableName, Headers, Rows]|FormattedRest]) :-
    table(TableName, Headers),
    table_output(RestTables, FormattedRest).
table_output([[TableName, Columns, Rows]|RestTables], [[TableName, Columns, Rows]|FormattedRest]) :-
    table_output(RestTables, FormattedRest).

table_command(TableColumnInfo, [], Results) :- get_table(TableColumnInfo, Results).
table_command(TableColumnInfo, [where, Conditions], Results) :-
    get_table(TableColumnInfo, Tables),
    process_sort(Tables, Conditions, Results).
table_command(TableColumnInfo, [matches, Values], Results) :-
    get_table(TableColumnInfo, Tables),
    match_up(Tables, Values, Results).
table_command(TableColumnInfo, [join, Table, Column], Results) :-
    apply_join(TableColumnInfo, Table, Column, Results).
table_command(TableColumnInfo, [matches, MatchCol, [command, [[MatchCol, Table]], [where, Condition]]], Results) :-
    evaluate_logical([command, [[MatchCol, Table]], [where, Condition]], SubResults),
    get_table(TableColumnInfo, Tables),
    match_up_part_two(Tables, SubResults, Results).

get_table([], []).
get_table([[all, TableName] | RestTables], [[TableName, Headers, Rows] | Results]) :-
    table(TableName, Headers),
    findall(CurrentRow, row(TableName, CurrentRow), UniqueRows),
    sort(UniqueRows, Rows),
    get_table(RestTables, Results).
get_table([[Columns, TableName]|RestTables], [[TableName, ColumnList, FilteredRows]|Results]) :-
    table(TableName, TableHeaders),
    (is_list(Columns) -> ColumnList = Columns ; ColumnList = [Columns]),
    findall(SelectedValues, (
        row(TableName, FullRow),
        get_values_from_row(FullRow, TableHeaders, ColumnList, SelectedValues)
    ), TempRows),
    sort(TempRows, FilteredRows),
    get_table(RestTables, Results).

match_up([], _, []).
match_up([[TableName, Columns, _]|RestTables], Values, [[TableName, FinalColumns, []]|Results]) :-
    table(TableName, TableHeaders),
    (Columns = all -> FinalColumns = TableHeaders ; FinalColumns = Columns),
    match_up(RestTables, Values, Results).

process_sort([], _, []).
process_sort([[TableName, Columns, _]|RestTables], Conditions, [[TableName, FinalColumns, UniqueRows]|Results]) :-
    table(TableName, TableHeaders),
    (Columns = all -> FinalColumns = TableHeaders ; FinalColumns = Columns),
    findall(SelectedValues, (
        row(TableName, FullRow),
        process_rule(TableHeaders, FullRow, Conditions),
        (Columns = all ->
            SelectedValues = FullRow
        ;
            get_values_from_row(FullRow, TableHeaders, Columns, SelectedValues)
        )
    ), TempRows),
    sort(TempRows, UniqueRows),
    process_sort(RestTables, Conditions, Results).

process_rule(_, _, []) :- !.
process_rule(Headers, Row, [condition, Column, Op, Value]) :-
    !,
    nth0(Index, Headers, Column),
    nth0(Index, Row, RowValue),
    evaluate_numbers(RowValue, Op, Value).

process_rule(Headers, Row, [and, Cond1, Cond2]) :-
    !,
    process_rule(Headers, Row, Cond1),
    process_rule(Headers, Row, Cond2).

process_rule(Headers, Row, [or, Cond1, Cond2]) :-
    !,
    (process_rule(Headers, Row, Cond1)
    ;
    process_rule(Headers, Row, Cond2)), !.

get_values_from_row(RowData, HeaderList, SelectedColumns, Values) :-
    findall(Extracted, (
        member(ColName, SelectedColumns),
        nth0(ColIndex, HeaderList, ColName),
        nth0(ColIndex, RowData, Extracted)
    ), Values).

evaluate_numbers(Val1, '=', Val2) :-
    (atom(Val1), atom(Val2) -> 
        Val1 = Val2
    ;
    numComp(Val1, Val2, '=') -> 
        true
    ;
    date_sort(Val1, Val2, '=')).

evaluate_numbers(Val1, Operator, Val2) :-
    member(Operator, ['>', '<']),
    (numComp(Val1, Val2, Operator) -> 
        true
    ;
    date_sort(Val1, Val2, Op)).

numComp(Val1, Val2, Op) :-
    catch((
        (atom(Val1) -> atom_to_number(Val1, Num1) ; number(Val1) -> Num1 = Val1),
        (atom(Val2) -> atom_to_number(Val2, Num2) ; number(Val2) -> Num2 = Val2),
        numEval(Num1, Num2, Op)
    ), _, fail).

atom_to_number(Atom, Number) :-
    atom_number(Atom, Number).

date_sort(Date1, Date2, Operator) :-
    safely_extract_date_components(Date1, ParsedDate1),
    safely_extract_date_components(Date2, ParsedDate2),
    evaluate_date_comparison(ParsedDate1, ParsedDate2, Operator).

safely_extract_date_components(DateString, [Year, Month, Day]) :-
    atom_string(DateString, ConvertedDate),  % Ensure input is a string.
    split_string(ConvertedDate, "-", "", [MonthStr, DayStr, YearStr]),  % Split date into parts.
    maplist(atom_number, [YearStr, MonthStr, DayStr], [Year, Month, Day]).  % Convert parts to numbers.

evaluate_date_comparison([Year1, Month1, Day1], [Year2, Month2, Day2], Operator) :-
    compute_date_value(Year1, Month1, Day1, Value1),
    compute_date_value(Year2, Month2, Day2, Value2),
    numEval(Value1, Value2, Operator).

compute_date_value(Year, Month, Day, Value) :-
    Value is Year * 10000 + Month * 100 + Day.

% numEval(+Value1, +Value2, +Operator)
% Evaluates the relationship between two numeric values.
numEval(Value1, Value2, '>') :- Value1 > Value2.
numEval(Value1, Value2, '<') :- Value1 < Value2.
numEval(Value1, Value2, '=') :- Value1 =:= Value2.

match_up_part_two([], _, []).
match_up_part_two([[TableName, Columns, _]|RestTables], _, [[TableName, FinalColumns, []]|Results]) :-
    table(TableName, TableHeaders),
    (Columns = all -> FinalColumns = TableHeaders ; FinalColumns = Columns),
    match_up_part_two(RestTables, [], Results).

apply_join([[Columns, SourceTable]], _, _, [[SourceTable, ColumnList, []]]) :-
    (is_list(Columns) -> ColumnList = Columns ; ColumnList = [Columns]).

extract_values(TargetColumn, [[_, Headers, Rows]|_], ExtractedValues) :-
    nth0(ColumnIndex, Headers, TargetColumn),
    findall(CurrentValue, (
        member(CurrentRow, Rows),
        nth0(ColumnIndex, CurrentRow, CurrentValue)
    ), TempValues),
    sort(TempValues, ExtractedValues).