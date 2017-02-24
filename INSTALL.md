# Installing Chronos


## Using erlang.mk

Just add
dep_chronos = https://github.com/lehoff/chronos v0.0.5
to your Makefile and it should be fine.



## Using rebar
If you are using rebar to build your project you should add the following to your dependencies:

    {chronos, "0.0.3", {git, "git://github.com/lehoff/chronos.git", {tag, "v0.0.3"}}}
