% Load required files
:- [facts].
:- [helper].

% Parse SQL-like natural language commands into a structured query
nlp_parse(LineSplit, Query) :-
    phrase(command(Query), LineSplit).

evaluate_logical([command, TableColumnInfo, Conditions], FilteredTable) :-
    print(TableColumnInfo),
    print(Conditions).
    % Process the table information
    % process_table_info(TableColumnInfo, TablesData),
    % Apply conditions for filtering rows
    % process_conditions(TablesData, Conditions, FilteredTable).

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
