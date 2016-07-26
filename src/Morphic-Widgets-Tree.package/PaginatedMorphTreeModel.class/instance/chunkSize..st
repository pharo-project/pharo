accessing
chunkSize: anIntegerOrNil
	self setPageSize: anIntegerOrNil.
	self announce: (MorphTreeChangeRequest chunkSize: anIntegerOrNil)
