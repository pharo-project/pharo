initialization
initialize
	super initialize.
	vertices := Array with: 5 @ 0 with: 20 @ 10 with: 0 @ 20.
	smoothCurve := false.
	self computeBounds.
