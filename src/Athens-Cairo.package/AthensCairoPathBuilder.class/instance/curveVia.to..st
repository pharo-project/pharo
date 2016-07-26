path commands
curveVia: p1 to: aPoint
	| pt0 pt1 cp1 cp2 twoThirds |
	"Quad bezier curve"

	pt0 := endPoint.
	pt1 := self toAbsolute: p1.
	endPoint := self toAbsolute: aPoint.
	lastControlPoint := nil.
	
"Any quadratic spline can be expressed as a cubic (where the cubic term is zero). The end points of the cubic will be the same as the quadratic's.

	CP0 = QP0
	CP3 = QP2

The two control points for the cubic are:

	CP1 = QP0 + 2/3 *(QP1-QP0)
	CP2 = QP2 + 2/3 *(QP1-QP2)"

	twoThirds := (2/3) asFloat.

	cp1 := pt1 - pt0 * twoThirds + pt0.
	cp2 := pt1 - endPoint * twoThirds + endPoint.
	
	self curveViaX: cp1 x Y: cp1 y viaX: cp2 x Y: cp2 y toX: endPoint x Y: endPoint y
