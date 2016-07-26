path commands
reflectedCurveVia: p2 to: aPoint
	|  pt1 |

	pt1 := lastControlPoint
		ifNil: [ endPoint ]
		ifNotNil: [ endPoint * 2 - lastControlPoint].
	lastControlPoint := self toAbsolute: p2.
	endPoint := self toAbsolute: aPoint.

	self curveViaX: pt1 x Y: pt1 y viaX: lastControlPoint x Y: lastControlPoint y toX: endPoint x Y: endPoint y
