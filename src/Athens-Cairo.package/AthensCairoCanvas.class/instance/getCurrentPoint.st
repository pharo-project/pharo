private
getCurrentPoint

	| x y |
	"since we use pointers to floats we must create a copy of original values to not scratch them"
	x := ByteArray new: 8.
	y := ByteArray new: 8.
	self primGetCurrentPointX: x Y: y.
	^ (x doubleAt: 1)@ (y doubleAt: 1)
