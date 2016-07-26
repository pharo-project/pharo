path segments visitor
visitCubicSegment: anAthensCubicSegment
	|  destination controlPoint controlPoint2 |
	destination := anAthensCubicSegment to.
	controlPoint := anAthensCubicSegment via1.
	controlPoint2 := anAthensCubicSegment via2.
	self
		curveToX: destination x Y: destination y
		viaX: controlPoint x Y: controlPoint y
		andX: controlPoint2 x Y: controlPoint2 y
		 		
