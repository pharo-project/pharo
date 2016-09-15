tests
testFirstEntryIfAbsent

	| absent |
	store firstEntryIfAbsent: [ absent := true ].
	self assert: absent.
	
	store newEntry: (OmEntry content: 42).
	store newEntry: (OmEntry content: 43).
	
	self 
		assert: (store firstEntryIfAbsent: [ self fail ])
		equals: store entries first