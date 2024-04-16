%%%---------------------------------------------------------------------
%%% @copyright 2024 William Fank Thomé
%%% @author William Fank Thomé <willilamthome@hotmail.com>
%%% @doc bel_css_3_selector_scan tests.
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
-module(bel_css_3_selector_scan_SUITE).

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
-export([ scan/1 ]).

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
    [ scan ].

%%%=====================================================================
%%% Test cases
%%%=====================================================================

scan(Config) when is_list(Config) ->
    Expect = [
        {hash,{1,1},<<"foo">>},
        {greater,{1,5}},
        {space,{1,7}},
        {'.',{1,8}},
        {ident,{1,9},<<"bar">>},
        {plus,{1,12}},
        {space,{1,14}},
        {ident,{1,15},<<"div">>},
        {'.',{1,18}},
        {ident,{1,19},<<"k1">>},
        {'.',{1,21}},
        {ident,{1,22},<<"k2">>},
        {space,{1,24}},
        {'[',{1,25}},
        {ident,{1,26},<<"id">>},
        {'=',{1,28}},
        {string,{1,29},<<"baz">>},
        {']',{1,34}},
        {':',{1,35}},
        {function,{1,36},<<"hello">>},
        {number,{1,42},<<"2">>},
        {')',{1,43}},
        {'not',{1,44}},
        {':',{1,49}},
        {function,{1,50},<<"where">>},
        {ident,{1,56},<<"div">>},
        {')',{1,59}},
        {')',{1,60}},
        {':',{1,61}},
        {':',{1,62}},
        {ident,{1,63},<<"before">>},
        {comma,{1,69}},
        {space,{1,70}},
        {hash,{1,71},<<"bar">>},
        {plus,{1,75}},
        {space,{1,77}},
        {'.',{1,78}},
        {ident,{1,79},<<"baz">>},
        {'.',{1,82}},
        {ident,{1,83},<<"fizz">>},
        {space,{1,87}},
        {ident,{1,88},<<"div">>},
        {'.',{1,91}},
        {ident,{1,92},<<"buzz">>}
    ],
    String = "#foo > .bar + div.k1.k2 [id='baz']:hello(2):not(:where(div))::before, #bar + .baz.fizz div.buzz",
    {ok, Expect, _} = bel_css_3_selector_scan:string(String),
    ok.

%%%=====================================================================
%%% Support functions
%%%=====================================================================

% nothing here yet!
