menu
arrowSpec: specPt
	"Specify a custom arrow for this line.
	specPt x abs gives the length of the arrow (point to base) in terms of borderWidth.
	If specPt x is negative, then the base of the arrow will be concave.
	specPt y abs gives the width of the arrow.
	The standard arrow is equivalent to arrowSpec: PolygonMorph defaultArrowSpec.
	See arrowBoundsAt:From: for details."

	self setProperty: #arrowSpec toValue: specPt.
	self computeBounds