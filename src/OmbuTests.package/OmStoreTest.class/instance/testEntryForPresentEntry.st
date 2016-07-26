tests
testEntryForPresentEntry
	| isPresent |
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	aReference := store referenceTo: anEntry.
	
	store 
		entryFor: aReference 
		ifPresent: [ :e | isPresent := true ] 
		ifAbsent: [ isPresent := false ].

	self assert: isPresent.