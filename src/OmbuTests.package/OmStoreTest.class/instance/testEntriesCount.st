tests
testEntriesCount
	self assert: store entriesCount equals: 0.
	
	store newEntry: (OmEntry content: 42).
	store newEntry: (OmEntry content: 43).
	
	self beforeAndAfterFlushStore: [
		self assert: store entriesCount equals: 2 ].