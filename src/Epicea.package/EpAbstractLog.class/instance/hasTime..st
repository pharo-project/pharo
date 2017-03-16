testing
hasTime: anEntry

	self 
		timeAt: anEntry 
		ifAbsent: [ ^ false ].
		
	^ true