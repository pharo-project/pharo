tests
testEntryForAbsentEntry
	| isPresent isAbsent |
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.

	self beforeAndAfterFlushStore: [
		isPresent := false.
		isAbsent := false.	

		otherStore 
			entryFor: (store referenceTo: anEntry)
			ifPresent: [ :e | isPresent := true ] 
			ifAbsent: [ isAbsent := true ].

		self deny: isPresent.
		self assert: isAbsent ]