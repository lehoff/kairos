# Installing Chronos

Chronos depends on gproc and has been tested with 0.2.12 as well as the latest master (b1855cb88 - as of 2012-02-25) after the 0.2.12 tag.

So you have to install gproc-0.2.12 in a place where your Erlang can find it.


## Using erlang.mk

Just add
dep_chronos = https://github.com/lehoff/chronos v0.0.5
to your Makefile and it should be fine.



## Using rebar
If you are using rebar to build your project you should add the following to your dependencies:

    {chronos, "0.0.3", {git, "git://github.com/lehoff/chronos.git", {tag, "v0.0.3"}}}

Chronos uses gproc and should you run into trouble with the `gproc`
dependecy Chronos will probably just work with the lastest version of
gproc, if not, please let me know and I will look into it.


