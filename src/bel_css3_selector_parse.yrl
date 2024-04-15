%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc CSS3 parser.
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

%% Output example for "#foo > .bar + div.k1.k2 [id='baz']:hello(2):not(:where(div))::before, #bar + .baz.fizz div.buzz"
%% [
%%     {greater,{
%%         [{id,"foo"}],
%%         {plus,{
%%             [{class,"bar"}],
%%             {space,{
%%                 [{type,{undefined,"div"}},{class,"k1"},{class,"k2"}],
%%                 [{attrib,{undefined,"id",{'=',{string,"'baz'"}}}},
%%                     {pseudo_class,{function,{"hello",[{number,"2"}]}}},
%%                     {negation,
%%                         {pseudo_class,{function,{"where",[{ident,"div"}]}}}},
%%                     {pseudo_class,{ident,"before"}}]
%%             }}
%%         }}
%%     }},
%%     {plus,{
%%         [{id,"bar"}],
%%         {space,{
%%             [{class,"baz"},{class,"fizz"}],
%%             [{type,{undefined,"div"}},{class,"buzz"}]
%%         }}
%%     }}
%% ].

Nonterminals
selectors_group selectors_combinator selector
combinator
simple_selector_sequence simple_selector_sequence_group
negation negation_arg
type_selector
namespace_prefix
element_name
universal
class
attrib attrib_namespace_prefix attrib_ident attrib_match attrib_match_left attrib_match_right
pseudo pseudo_class pseudo_element functional_pseudo
expressions expression
id
.

Terminals
includes
dash_match prefix_match suffix_match substring_match
plus greater tilde
comma space
not
% cdo cdc
hash ident string
function
% at_keyword
dimension number % percentage
'*' '.' '[' ']' '|' ':' ')' '-' '='
.

Rootsymbol
selectors_group
.

selectors_group -> selector                                         : ['$1'].
selectors_group -> selectors_combinator                             : ['$1'].
selectors_group -> selector comma selectors_group                   : ['$1' | '$3'].
selectors_group -> selector comma space selectors_group             : ['$1' | '$4'].
selectors_group -> selectors_combinator comma selectors_group       : ['$1' | '$3'].
selectors_group -> selectors_combinator comma space selectors_group : ['$1' | '$4'].

selectors_combinator -> selector combinator space selector             : {'$2', {'$1', '$4'}}.
selectors_combinator -> selector combinator space selectors_combinator : {'$2', {'$1', '$4'}}.
selectors_combinator -> selector space selector                        : {space, {'$1', '$3'}}.
selectors_combinator -> selector space selectors_combinator            : {space, {'$1', '$3'}}.

selector -> simple_selector_sequence_group          : ['$1'].
selector -> simple_selector_sequence_group selector : ['$1' | '$2'].

simple_selector_sequence_group -> type_selector            : '$1'.
simple_selector_sequence_group -> universal                : '$1'.
simple_selector_sequence_group -> simple_selector_sequence : '$1'.

simple_selector_sequence -> id       : '$1'.
simple_selector_sequence -> class    : '$1'.
simple_selector_sequence -> attrib   : '$1'.
simple_selector_sequence -> pseudo   : '$1'.
simple_selector_sequence -> negation : '$1'.

combinator -> plus    : plus.
combinator -> greater : greater.
combinator -> tilde   : tilde.

negation -> not negation_arg ')'             : {negation, '$2'}.
negation -> not space negation_arg ')'       : {negation, '$3'}.
negation -> not negation_arg space ')'       : {negation, '$2'}.
negation -> not space negation_arg space ')' : {negation, '$3'}.

negation_arg -> type_selector : '$1'.
negation_arg -> universal     : '$1'.
negation_arg -> id            : '$1'.
negation_arg -> class         : '$1'.
negation_arg -> attrib        : '$1'.
negation_arg -> pseudo        : '$1'.

type_selector -> namespace_prefix element_name : {type, {'$1', '$2'}}.
type_selector -> element_name                  : {type, {undefined, '$1'}}.

namespace_prefix -> ident '|' : {namespace_prefix, value_of('$1')}.
namespace_prefix -> '*' '|'   : {namespace_prefix, '*'}.
% FIXME: Conflict in the namespace_prefix below.
%        See: https://www.w3.org/TR/2018/REC-selectors-3-20181106/#typenmsp
% namespace_prefix -> '|'       : {namespace_prefix, undefined}.

element_name -> ident : value_of('$1').

universal -> namespace_prefix '*' : {universal, '$1'}.
universal -> '*'                  : {universal, undefined}.

class -> '.' ident : {class, value_of('$2')}.

attrib -> '[' attrib_ident ']'                                      : {attrib, {undefined, '$2', undefined}}.
attrib -> '[' attrib_namespace_prefix attrib_ident ']'              : {attrib, {'$2', '$3', undefined}}.
attrib -> '[' attrib_ident attrib_match ']'                         : {attrib, {undefined, '$2', '$3'}}.
attrib -> '[' attrib_namespace_prefix attrib_ident attrib_match ']' : {attrib, {'$2', '$3', '$4'}}.

attrib_namespace_prefix -> space namespace_prefix : '$2'.
attrib_namespace_prefix -> namespace_prefix       : '$1'.

attrib_ident -> ident space : value_of('$1').
attrib_ident -> ident       : value_of('$1').

attrib_match -> attrib_match_left attrib_match_right             : {'$1', '$2'}.
attrib_match -> attrib_match_left space attrib_match_right       : {'$1', '$3'}.
attrib_match -> attrib_match_left attrib_match_right space       : {'$1', '$2'}.
attrib_match -> attrib_match_left space attrib_match_right space : {'$1', '$3'}.

attrib_match_left -> prefix_match    : prefix_match.
attrib_match_left -> suffix_match    : suffix_match.
attrib_match_left -> substring_match : substring_match.
attrib_match_left -> '='             : '='.
attrib_match_left -> includes        : includes.
attrib_match_left -> dash_match      : dash_match.

attrib_match_right -> ident  : {ident, value_of('$1')}.
attrib_match_right -> string : {string, value_of('$1')}.

pseudo -> pseudo_class   : '$1'.
pseudo -> pseudo_element : '$1'.

% FIXME: :where and :is are not correct.
%        See: https://developer.mozilla.org/en-US/docs/Web/CSS/:where
pseudo_class -> ':' ident             : {pseudo_class, {ident, value_of('$2')}}.
pseudo_class -> ':' functional_pseudo : {pseudo_class, '$2'}.

pseudo_element -> ':' ':' ident             : normalize_pseudo_element('$3').
pseudo_element -> ':' ':' functional_pseudo : {pseudo_element, '$3'}.

functional_pseudo -> 'function' expressions ')'       : {'function', {value_of('$1'), '$2'}}.
functional_pseudo -> 'function' space expressions ')' : {'function', {value_of('$1'), '$3'}}.

expressions -> expression                   : ['$1'].
expressions -> expression expressions       : ['$1' | '$2'].
expressions -> expression space expressions : ['$1' | '$3'].

expression -> plus      : plus.
expression -> '-'       : '-'.
expression -> dimension : {dimension, value_of('$1')}.
expression -> number    : {number, value_of('$1')}.
expression -> string    : {string, value_of('$1')}.
expression -> ident     : {ident, value_of('$1')}.

id -> hash : {id, value_of('$1')}.

Erlang code.

value_of(Token) when is_tuple(Token), tuple_size(Token) =:= 3 ->
    element(3, Token).

normalize_pseudo_element(Token) ->
    Value = value_of(Token),
    Exceptions = ["first-line", "first-letter", "before", "after"],
    case lists:member(Value, Exceptions) of
        true ->
            {pseudo_class, {ident, Value}};
        false ->
            {pseudo_element, {ident, Value}}
    end.
