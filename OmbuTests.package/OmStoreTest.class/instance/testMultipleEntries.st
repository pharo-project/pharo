tests
testMultipleEntries
	1 to: 7 do: [ :each |
		store newEntry: (OmEntry content: each) ].
	
	self 
		assert: (store entries collect: #content as: Set)
		equals: (1 to: 7) asSet