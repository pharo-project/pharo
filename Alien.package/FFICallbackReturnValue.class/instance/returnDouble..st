instance initalization
returnDouble: f
	self signedLongAt: self typeOffset put: 2.
	self doubleAt: self valueOffset put: f asFloat