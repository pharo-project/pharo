accessing
pageSize: anIntegerOrNil
	self setPageSize: anIntegerOrNil.
	self announce: (MorphTreeChangeRequest pageSize: anIntegerOrNil)
