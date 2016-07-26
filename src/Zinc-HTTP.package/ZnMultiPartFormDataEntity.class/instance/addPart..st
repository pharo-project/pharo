accessing
addPart: mimePart
	self invalidateRepresentation.
	self parts add: mimePart