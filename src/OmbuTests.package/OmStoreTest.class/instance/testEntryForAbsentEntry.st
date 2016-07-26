tests
testEntryForAbsentEntry
	| isPresent |
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	aReference := store referenceTo: anEntry.
	
	otherStore 
		entryFor: aReference 
		ifPresent: [ :e | isPresent := true ] 
		ifAbsent: [ isPresent := false ].

	self deny: isPresent.