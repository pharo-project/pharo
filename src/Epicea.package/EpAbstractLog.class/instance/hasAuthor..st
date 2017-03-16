testing
hasAuthor: anEntry

	self 
		authorAt: anEntry 
		ifAbsent: [ ^ false ].
		
	^ true