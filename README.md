graphviz: Visualize graphs
==========================

This package contains libraries to visualize graphs with
[Graphviz](http://www.graphviz.org/).

There is also a convenient operation `Data.GraphViz.viewDotGraph`
to visualize a graph with the `dotviewcommand` specified in
the rc file of the Curry system.
Thus, this operation looks up the value of the `dotviewcommand` field
in the rc file (e.g., `~/.pakcsrc` in case of PAKCS) and passes
the text of specified graph to this command.
Here are some reasonable settings of this field:

    dotviewcommand=dot -Tpdf > /tmp/dotxxx.pdf && evince /tmp/dotxxx.pdf
    dotviewcommand=neato -Tpdf > /tmp/dotxxx.pdf && evince /tmp/dotxxx.pdf
    dotviewcommand=circo -Tpdf > /tmp/dotxxx.pdf && evince /tmp/dotxxx.pdf
    dotviewcommand=fdp -Tpdf > /tmp/dotxxx.pdf && evince /tmp/dotxxx.pdf

--------------------------------------------------------------------------
