# Installing Chronos

Chronos depends on gproc and has been tested with 0.2.12 as well as the latest master (b1855cb88 - as of 2012-02-25) after the 0.2.12 tag.

So you have to install gproc-0.2.12 in a place where your Erlang can find it.

The dependency is documented in the `sinan.config` file.

## Using sinan

It should work - I use sinan myself and it works for me.



## Using rebar
If you are using rebar to build your project you should add the following to your dependencies:

    {chronos, "0.0.2", {git, "git://github.com/lehoff/chronos.git", {tag, "v0.0.2"}}},
    {gproc, "0.2.12", {git, "https://github.com/uwiger/gproc.git", {tag,"v0.2.10"}}}

Should you run into trouble with the `gproc` dependecy Chronos will probably just work with the lastest version of gproc, if not, please let me know and I will look into it.
