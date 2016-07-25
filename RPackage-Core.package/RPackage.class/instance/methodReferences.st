system compatibility
methodReferences
	
	^ self methods collect: [:each | each methodReference] thenSelect: [:each| each isValid and: [each isLocalSelector]].