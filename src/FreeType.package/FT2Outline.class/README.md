@instVar: contoursSize - The number of contours in the outline.
@instVar: pointsSize - The number of points in the outline.
@instVar: points - an array of  26.6 fixed point integer pairs giving the outline's point coordinates.

@instVar: tags	- an array of pointsSize bytes, giving each outline point's type. 

(counting from 0)

If bit 0 is unset, the point is 'off' the curve, i.e., a Bézier control point, while it is 'on' when set.

Bit 1 is meaningful for 'off' points only. If set, it indicates a third-order Bézier arc control point; and a second-order control point if unset.

@instVar: contours - an array of contoursSize shorts, giving the end point of each contour within the outline. For example, the first contour is defined by the points '0' to 'contours[0]', the second one is defined by the points 'contours[0]+1' to 'contours[1]', etc.

@instVar: flags - a set of bit flags used to characterize the outline and give hints to the scan-converter and hinter on how to convert/grid-fit it.