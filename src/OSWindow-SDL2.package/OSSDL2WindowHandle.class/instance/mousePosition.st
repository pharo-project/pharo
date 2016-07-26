event handling
mousePosition
	| x y |
	x := ByteArray new: 4.
	y := ByteArray new: 4.
	SDL2 mouseStateX: x y: y.
	^ ( x signedLongAt: 1) @ (y signedLongAt: 1)