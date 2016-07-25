path segments visitor
visitQuadSegment: anAthensCubicSegment
	| destination controlPoint |
	destination := anAthensCubicSegment to.
	controlPoint := anAthensCubicSegment via.
	self
		curveToX: destination x Y: destination y
		viaX: controlPoint x Y: controlPoint y
		 