initialization
remove: anObject ifAbsent: aBlock

	collection remove: anObject ifAbsent: aBlock.
	self announce: GLMItemRemoved