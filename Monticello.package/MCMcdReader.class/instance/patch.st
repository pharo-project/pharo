accessing
patch
	^ patch ifNil: [self loadPatch]