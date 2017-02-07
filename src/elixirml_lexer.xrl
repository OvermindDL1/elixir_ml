
Definitions.

Whitespace     = [\s\t\n\r]+
Comment        = \(\*.*\*\)
Float          = [0-9]*\.[0-9]*
Integer        = [0-9]+
Operators      = [-+*/%><!|&=]+
LetMacroCall   = let%
Keyword        = let|rec|in|and|match|when|[\(\)\[\]\{\}]|,|:
Identifier     = [_A-Za-z][_A-Za-z0-9]*


Rules.

{Whitespace}               : {token, {whitespace, TokenLine, TokenChars}}.
{Float}                    : {token, {float, TokenLine, list_to_float(TokenChars)}}.
{Integer}                  : {token, {integer, TokenLine, list_to_integer(TokenChars)}}.
{Operators}                : {token, {operator, TokenLine, list_to_atom(TokenChars)}}.
{LetMacroCall}{Identifier} : {token, {'let%', TokenLine, lists:nthtail(4, TokenChars)}}.
{Keyword}                  : {token, {list_to_atom(TokenChars), TokenLine}}.
{Identifier}               : {token, {identifier, TokenLine, TokenChars}}.


Erlang code.
