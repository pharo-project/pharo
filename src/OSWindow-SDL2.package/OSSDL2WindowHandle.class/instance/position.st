accessing
position
	| x y |
	x := ByteArray new: ExternalType long byteSize.
	y := ByteArray new: ExternalType long byteSize.
	handle getPositionX: x y: y.
	^ ( x signedLongAt: 1) @ (y signedLongAt: 1)