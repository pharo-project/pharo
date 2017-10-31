tests
testEntryForPresentEntry
	| isPresent isAbsent |
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.

	self beforeAndAfterFlushStore: [
		isPresent := false.
		isAbsent := false.	

		store 
			entryFor: (store referenceTo: anEntry)
			ifPresent: [ :e | isPresent := true ] 
			ifAbsent: [ isAbsent := true ].

		self assert: isPresent.
		self deny: isAbsent ]