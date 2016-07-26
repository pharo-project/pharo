I represent a single text line, which belongs to specific layout.

I carry various geometrical information, such as maximum ascent, line height and width.
My commands are TxDisplayCommand-s which contain enough information to render
the line correctly on canvas , as well as for measuring font metrics.

I am certainly not the top level API and my implementation is considered private
(in contrast to LayoutView).