%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc CSS3 scanner.
%%%
%%% Copyright 2024 William Fank Thomé
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%%     http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%
%%% @end
%%%---------------------------------------------------------------------

% Source: https://www.w3.org/TR/2018/REC-selectors-3-20181106/#lex

Definitions.

W         = ([\s\t\r\n\f]*)
% TODO: Use yecc to deal with {1,6} because it's not supported by lexer regex.
% UNICODE = (\\[0-9a-f]{1,6}(\r\n|{W})?)
UNICODE   = (\\[0-9a-f]+(\r\n|{W})?)
ESCAPE    = ({UNICODE}|\\[^\n\r\f0-9a-f])
NONASCII  = [^\x00-\x7F]
NMSTART   = ([_a-z]|{NONASCII}|{ESCAPE})
NMCHAR    = ([_a-z0-9-]|{NONASCII}|{ESCAPE})
IDENT     = ([-]?{NMSTART}{NMCHAR}*)
NAME      = ({NMCHAR}+)
NUM       = ([0-9]+|[0-9]*\.[0-9]+)
NL        = (\n|\r\n|\r|\f)
STRING1   = (\"([^\n\r\f\\"]|\\{NL}|{NONASCII}|{ESCAPE})*\")
STRING2   = (\'([^\n\r\f\\']|\\{NL}|{NONASCII}|{ESCAPE})*\')
STRING    = {STRING1}|{STRING2}
INVALID1  = (\"([^\n\r\f\\"]|\\{NL}|{NONASCII}|{ESCAPE})*)
INVALID2  = (\'([^\n\r\f\\']|\\{NL}|{NONASCII}|{ESCAPE})*)
INVALID   = {INVALID1}|{INVALID2}

% TODO: Use yecc to deal with {0,4} because it's not supported by lexer regex.
% TODO: Check what's D, E and V and why they aren't used.
% D         = d|\\x00*(\x44|\x64)({NL}|{W})?
% E         = e|\\x00*(\x45|\x65)({NL}|{W})?
N         = (n|\\x00*(\x4e|\x6e)({NL}|{W})?|\\n)
O         = (o|\\x00*(\x4f|\x6f)({NL}|{W})?|\\o)
T         = (t|\\x00*(\x54|\x74)({NL}|{W})?|\\t)
% V         = (v|\\x00*(\x58|\x78)({NL}|{W})?|\\v)

SKIP      = \/\*[^*]*\*+([^/*][^*]*\*+)*\/

Rules.

% TODO: Commented rules are not used by yecc.
\~\=          : {token, {includes, TokenLoc}}. % INCLUDES;
\|\=          : {token, {dash_match, TokenLoc}}. % DASHMATCH;
\^\=          : {token, {prefix_match, TokenLoc}}. % PREFIXMATCH;
\$\=          : {token, {suffix_match, TokenLoc}}. % SUFFIXMATCH;
\*\=          : {token, {substring_match, TokenLoc}}. % SUBSTRINGMATCH;
{W}\+         : {token, {plus, TokenLoc}}. % PLUS;
{W}\>         : {token, {greater, TokenLoc}}. % GREATER;
{W}\,         : {token, {comma, TokenLoc}}. % COMMA;
{W}\~         : {token, {tilde, TokenLoc}}. % TILDE;
{W}+          : {token, {space, TokenLoc}}. % S;
\:{N}{O}{T}\( : {token, {'not', TokenLoc}}. % NOT;
{INVALID}     : {error, "invalid syntax: " ++ TokenChars}. % INVALID;
% \<\!\-\-      : {token, {cdo, TokenLoc}}. % CDO;
% \-\-\>        : {token, {cdc, TokenLoc}}. % CDC;
\#{NAME}      : {token, {hash, TokenLoc, tl(TokenChars)}}. % HASH;
{NUM}{IDENT}  : {token, {dimension, TokenLoc, TokenChars}}. % DIMENSION;
{IDENT}\(     : {token, {'function', TokenLoc, lists:droplast(TokenChars)}}. % FUNCTION;
% \@{IDENT}     : {token, {at_keyword, TokenLoc, TokenChars}}. % ATKEYWORD;
{IDENT}       : {token, {ident, TokenLoc, TokenChars}}. % IDENT;
{NUM}\%       : {token, {percentage, TokenLoc, TokenChars}}. % PERCENTAGE;
{NUM}         : {token, {number, TokenLoc, TokenChars}}. % NUMBER;
{STRING}      : {token, {string, TokenLoc, TokenChars}}. % STRING;
SKIP          : skip_token.

% NOTE: The symbols below aren't in the source grammar.
\*            : {token, {any, TokenLoc}}.
\.            : {token, {'.', TokenLoc}}.
\[            : {token, {'[', TokenLoc}}.
\]            : {token, {']', TokenLoc}}.
\|            : {token, {'|', TokenLoc}}.
\:            : {token, {':', TokenLoc}}.
\)            : {token, {')', TokenLoc}}.
\-            : {token, {'-', TokenLoc}}.
\=            : {token, {'=', TokenLoc}}.

Erlang code.

% nothing here yet!
