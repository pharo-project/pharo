This morph can be used to show a list having multiple columns,  The columns are self width sized to make the largest entry in each list fit.  In some cases the pane may then be too narrow.

Use it like a regular PluggableListMorph except pass in an array of lists instead of a single list.

There are base assumptions made here that each list in the array of lists is the same size.

Also, the highlight color for the selection is easy to modify in the #highlightSelection method.  I used blue
when testing just to see it work.