erlang-rison
============


An Erlang module for encoding/decoding [Rison](http://mjtemplate.org/examples/rison.html)
(a data serialization format optimized for compactness in URIs).


Quick start
-----------

    $ make
    ...
    $ erl -pa ebin
    ...
    1> rison:load("!t").
    {ok, true}
    2> rison:load("!(1,2,3)").
    {ok,{array,[1,2,3]}}
    3> rison:load("123.456e789").
    {ok,{number,123,'456',789}}
    4> rison:load("abc def").
    {error,invalid_input}
    5> rison:dump({array,[1,2,3]}).
    {ok,"!(1,2,3)"}
    6> rison:dump({object,[{a,0}]}).
    {ok,"(a:0)"}
    7> rison:dump({}).
    {error,invalid_input}


Use rison:load/1 to decode Rison into Erlang terms. Use rison:dump/1 to encode
Erlang terms as Rison.
