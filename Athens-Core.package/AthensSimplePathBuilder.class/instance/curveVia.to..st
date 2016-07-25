path commands
curveVia: cp1 to: aPoint
	| pt1 pt2 |
	"Quad bezier curve"

	pt1 := self toAbsolute: cp1.
	pt2 := self toAbsolute: aPoint.
	^ self addSegment: (
		AthensQuadSegment new 
			from: lastSegment endPoint 
			via: pt1 
			to: pt2)

