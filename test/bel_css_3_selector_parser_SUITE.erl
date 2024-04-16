%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc bel_css_3_selector_parser tests.
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
-module(bel_css_3_selector_parser_SUITE).

% -include_lib("common_test/include/ct.hrl").

%% Callback functions
-export([ suite/0
        , all/0
        , init_per_suite/1
        , end_per_suite/1
        , init_per_testcase/2
        , end_per_testcase/2
        ]).

%% Test cases
-export([ parse/1 ]).

%%%=====================================================================
%%% Callback functions
%%%=====================================================================

%%----------------------------------------------------------------------
%% @doc Returns list of tuples to set default properties for the suite.
%%
%% @param Info List of key/value pairs.
%%
%% @end
%%----------------------------------------------------------------------
-spec suite() -> Info when
    Info :: [tuple()].

suite() ->
    [].

%%----------------------------------------------------------------------
%% @doc Initialization before the suite.
%%
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec init_per_suite(Config0) -> Config when
    Config0 :: [tuple()],
    Config :: [tuple()].

init_per_suite(Config) ->
    Config.

%%----------------------------------------------------------------------
%% @doc Cleanup after the suite.
%%
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec end_per_suite(Config) -> Result when
    Config :: [tuple()],
    Result :: term().

end_per_suite(_Config) ->
    ok.

%%----------------------------------------------------------------------
%% @doc Initialization before each test case.
%%
%% @param TestCase Name of the test case that is about to run.
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec init_per_testcase(TestCase, Config0) -> Config when
    TestCase :: atom(),
    Config0 :: [tuple()],
    Config :: [tuple()].

init_per_testcase(_TestCase, Config) ->
    Config.

%%----------------------------------------------------------------------
%% @doc Cleanup after each test case.
%%
%% @param TestCase Name of the test case that is finished.
%% @param Config A list of key/value pairs, holding the test case configuration.
%%
%% @end
%%----------------------------------------------------------------------
-spec end_per_testcase(TestCase, Config) -> Result when
    TestCase :: atom(),
    Config :: [tuple()],
    Result :: term().

end_per_testcase(_TestCase, _Config) ->
    ok.

%%----------------------------------------------------------------------
%% @doc Returns the list of groups and test cases that are to be executed.
%%
%% @param GroupName Name of a test case group.
%% @param TestCase Name of a test case.
%%
%% @end
%%----------------------------------------------------------------------
-spec all() -> GroupsAndTestCases when
    GroupsAndTestCases :: [Group | TestCase],
    Group :: {group, GroupName},
    GroupName :: atom(),
    TestCase :: atom().

all() ->
    [ parse ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

parse(Config) when is_list(Config) ->
    Expect = [
        {greater,
            {[{id,<<"foo">>}],
            {plus,
                {[{class,<<"bar">>}],
                {space,
                    {[{type,{undefined,<<"div">>}},
                        {class,<<"k1">>},
                        {class,<<"k2">>}],
                        [{attrib,
                            {undefined,<<"id">>,{'=',{string,<<"baz">>}}}},
                        {pseudo_class,
                            {function,{<<"hello">>,[{number,<<"2">>}]}}},
                        {negation,
                            {pseudo_class,
                                {function,
                                    {<<"where">>,[{ident,<<"div">>}]}}}},
                        {pseudo_element,{ident,<<"before">>}}]}}}}}},
        {plus,
            {[{id,<<"bar">>}],
            {space,
                {[{class,<<"baz">>},{class,<<"fizz">>}],
                [{type,{undefined,<<"div">>}},{class,<<"buzz">>}]}}}}
    ],
    String = "#foo > .bar + div.k1.k2 [id='baz']:hello(2):not(:where(div))::before, #bar + .baz.fizz div.buzz",
    {ok, Tokens, _} = bel_css_3_selector_scan:string(String),
    {ok, Expect} = bel_css_3_selector_parser:parse(Tokens),
    ok.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

% nothing here yet!
