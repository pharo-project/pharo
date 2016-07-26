accessing
at: key put: anObject
	self checkLimitForKey: key.
	^ super at: key put: anObject