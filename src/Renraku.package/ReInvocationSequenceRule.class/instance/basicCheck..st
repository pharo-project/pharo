running
basicCheck: anEntity
	
	 self
		check: anEntity
		forCritiquesDo: [ ^ true ]
		ifNone: [ ^ false ]