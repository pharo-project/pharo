vector-transform
transform: aPoint
	| x y |
	x := ByteArray new: 8.
	y := ByteArray new: 8.
	x doubleAt: 1 put: aPoint x.
	y doubleAt: 1 put: aPoint y.
	self primTransformX: x Y: y.
	^ (x doubleAt: 1) @ (y doubleAt: 1)