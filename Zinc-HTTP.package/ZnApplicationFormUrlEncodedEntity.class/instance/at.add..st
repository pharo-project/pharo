accessing
at: key add: value
	self invalidateRepresentation.
	^ self fields at: key add: value