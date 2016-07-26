balloon drawing
drawGeneralBezier3Shape: contours color: c borderWidth: borderWidth
borderColor: borderColor
	| b2 |
	b2 := contours collect: [:b3 | Bezier3Segment
convertBezier3ToBezier2: b3 ].
	self drawGeneralBezierShape: b2 color: c borderWidth: borderWidth
borderColor: borderColor