Nonterminals
expr
.


Terminals
'let%' let rec in and match when '(' ')' '[' ']' '{' '}' ',' ':'
integer float
operator
identifier
.

Rootsymbol expr.

%expr -> let identifier '=' expr in : {binding, '$2', '$3'}
%expr -> '(' operator arg_list ')' : {expr, value_of('$2'), '$3'}.
expr -> integer : ok.


Erlang code.

-define(l2i(L), list_to_integer(L)).
value_of({_,_,V}) -> V.
line_of({_,V,_}) -> V.
