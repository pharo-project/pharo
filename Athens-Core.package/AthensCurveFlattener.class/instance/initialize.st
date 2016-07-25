initialize-release
initialize

	super initialize.
	
	transform := AthensAffineTransform new. "identity"
	self toleranceMultiplier: self defaultTolerance