path commands
curveVia: cp1 and: cp2 to: aPoint
	"Cubic bezier curve"
	
	| pt1 pt2 pt3 |
	"Quad bezier curve"

	pt1 := self toAbsolute: cp1.
	pt2 := self toAbsolute: cp2.
	pt3 := self toAbsolute: aPoint.
	^ self addSegment: (
		AthensCubicSegment new 
			from: lastSegment endPoint 
			via: pt1
			and: pt2
			to: pt3)

