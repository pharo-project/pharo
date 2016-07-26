override
at: key put: anObject

	value at: key put: anObject.
	self valueAdded: anObject.
	^ anObject