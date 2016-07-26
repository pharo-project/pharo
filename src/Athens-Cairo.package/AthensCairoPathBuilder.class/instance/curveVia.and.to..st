path commands
curveVia: p1 and: p2 to: aPoint
	|  pt1 |

	pt1 := self toAbsolute: p1.
	lastControlPoint := self toAbsolute: p2.
	endPoint := self toAbsolute: aPoint.

	self curveViaX: pt1 x Y: pt1 y viaX: lastControlPoint x Y: lastControlPoint y toX: endPoint x Y: endPoint y
