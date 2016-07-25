accessing
at: key put: value
	self invalidateRepresentation.
	^ self fields at: key put: value