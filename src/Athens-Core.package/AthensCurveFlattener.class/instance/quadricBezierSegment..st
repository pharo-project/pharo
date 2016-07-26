visiting
quadricBezierSegment: segment
	| pt1 pt2 pt3 |
	
	pt1 := transform transform: endPoint.
	pt2 := transform transform: segment via.
	pt3 := transform transform: segment endPoint.
	
	endPoint := segment endPoint.

	self flattenQuadBezier: pt1 x y1: pt1 y x2: pt2 x y2: pt2 y x3: pt3 x y3: pt3 y.
