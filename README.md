# bel_css

CSS utilities for Erlang.

## Scanner and Parser

### Example

```erlang
1> Selectors = "#foo > .bar + div.k1.k2 [id='baz']:hello(2):not(:where(div))::before, #bar + .baz.fizz div.buzz".
"#foo > .bar + div.k1.k2 [id='baz']:hello(2):not(:where(div))::before, #bar + .baz.fizz div.buzz"

2> {ok, Tokens, _} = bel_css_3_selector_scan:string(Selectors).
{ok,[{hash,{1,1},"foo"},
     {greater,{1,5}},
     {space,{1,7}},
     {'.',{1,8}},
     {ident,{1,9},"bar"},
     {plus,{1,12}},
     {space,{1,14}},
     {ident,{1,15},"div"},
     {'.',{1,18}},
     {ident,{1,19},"k1"},
     {'.',{1,21}},
     {ident,{1,22},"k2"},
     {space,{1,24}},
     {'[',{1,25}},
     {ident,{1,26},"id"},
     {'=',{1,28}},
     {string,{1,29},"'baz'"},
     {']',{1,34}},
     {':',{1,35}},
     {function,{1,36},"hello"},
     {number,{1,42},"2"},
     {')',{1,43}},
     {'not',{1,44}},
     {':',{1,49}},
     {function,{1,50},"where"},
     {ident,{1,56},"div"},
     {')',{1,59}},
     {')',{1,60}},
     {':',{1,61}},
     {':',{1,62}},
     {ident,{1,63},"before"},
     {comma,{1,69}},
     {space,{1,70}},
     {hash,{1,71},"bar"},
     {plus,{1,75}},
     {space,{1,77}},
     {'.',{1,78}},
     {ident,{1,79},"baz"},
     {'.',{1,82}},
     {ident,{1,83},"fizz"},
     {space,{1,87}},
     {ident,{1,88},"div"},
     {'.',{1,91}},
     {ident,{1,92},"buzz"}],
    1}

3> bel_css_3_selector_parse:parse(Tokens).
{ok,[{greater,
         {[{id,"foo"}],
          {plus,
              {[{class,"bar"}],
               {space,
                   {[{type,{undefined,"div"}},{class,"k1"},{class,"k2"}],
                    [{attrib,{undefined,"id",{'=',{string,"'baz'"}}}},
                     {pseudo_class,{function,{"hello",[{number,"2"}]}}},
                     {negation,
                         {pseudo_class,{function,{"where",[{ident,"div"}]}}}},
                     {pseudo_class,{ident,"before"}}]}}}}}},
     {plus,
         {[{id,"bar"}],
          {space,
              {[{class,"baz"},{class,"fizz"}],
               [{type,{undefined,"div"}},{class,"buzz"}]}}}}]}
```

## Build

```shell
$ rebar3 compile
```
