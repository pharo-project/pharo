initialization
initialize
	
	super initialize.
	vertices := Array
				with: 5 @ 0
				with: 20 @ 10
				with: 0 @ 20.
	closed := true.
	smoothCurve := false.
	arrows := #none.
	self computeBounds.
	self beSmoothCurve.
	self diamondOval.