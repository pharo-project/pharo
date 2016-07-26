This class implements a morph which can behave as four different objects depending on the the following two facts:
- is it OPEN or CLOSED?
- is it SEGMENTED or SMOOTHED.

1. The OPEN and SEGMENTED variant looks like polyline.

2. The OPEN and SMOOTHED variant looks like spline (kind of curve)

3. The CLOSED and SEGMENTED variant looks like polygon. This is actually what you get when you do
	PolygonMorph new openInWorld
You get a triangle. See below how to manipulate these objects...

4. The CLOSED and SMOOTHED variant looks like blob (???)

Prototypes of this morph can also be found in "Object Catalog". Several (different variants) of this object are among "Basic" morphs.

Explore the assiciated morph-menu. It enables you
- to toggle showing of "handles". They make it possible to
	- reposition already existing vertices (by moving yellow handles)
	- create new vertices (by moving green handles)
	- delete already existing vertices (by dragging and dropping one yellow handle closely
	  nearby the adjacent yellow handle
  Handles can be made visible/hidden by shift+leftclicking the morph. This way it is possible
  to quickly show handles, adjust vertices and then again hide handles.
- making closed polygon open, i.e. converting it to a curve (and vice versa)
- toggle smoothed/segmented line/outline
- set up custom dashing (for line, curves or borders of closed polygons
- set up custom arrow-heads (for lines resp. curves)

------------------------------------------------------------------------------------------
Implementation notes:

This class combines the old Polygon and Curve classes.

The 1-bit fillForm to make display and containment tests reasonably fast.  However, this functionality is in the process of being supplanted by balloon capabilities, which should eventually provide anti-aliasing as well.

wiz 7/18/2004 21:26
s have made some changes to this class to

1) correct some bugs associated with one vertex polygons.

2) prepare for some enhancements with new curves.

3) add shaping items to menu.