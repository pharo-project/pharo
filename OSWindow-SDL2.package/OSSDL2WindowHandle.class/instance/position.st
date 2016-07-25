accessing
position
	| x y |
	x := ByteArray new: 4.
	y := ByteArray new: 4.
	handle getPositionX: x y: y.
	^ ( x signedLongAt: 1) @ (y signedLongAt: 1)