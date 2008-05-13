===============
Rison In Erlang
===============


What is this?
-------------

A Rison encoder/decoder written in Erlang.


What is Rison?
--------------

A data serialization format optimized for compactness in URIs.

See http://mjtemplate.org/examples/rison.html for more information.


How do I use it?
----------------

To convert Rison-formatted strings into Erlang terms, use rison:load/1.
To do the opposite, use rison:dump/1. For example:

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


Type "make i" to compile the code and get an Erlang shell where you
can try out these examples.