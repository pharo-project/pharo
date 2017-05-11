tests
testEntryForNullReference
	| isPresent isAbsent |

	self beforeAndAfterFlushStore: [
		isPresent := false.
		isAbsent := false.	

		otherStore 
			entryFor: OmNullReference uniqueInstance
			ifPresent: [ :e | isPresent := true ] 
			ifAbsent: [ isAbsent := true ].

		self deny: isPresent.
		self assert: isAbsent ]