tests
testFirstEntryIfAbsent

	| absent |
	absent := false.
	store firstEntryIfAbsent: [ absent := true ].
	self assert: absent.
	
	store newEntry: (OmEntry content: 42).
	store newEntry: (OmEntry content: 43).
	
	self beforeAndAfterFlushStore: [
		self 
			assert: (store firstEntryIfAbsent: [ self fail ])
			equals: store entries first ]