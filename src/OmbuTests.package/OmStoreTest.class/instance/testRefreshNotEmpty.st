tests
testRefreshNotEmpty
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	store refresh.
	
	self 
		assert: (store entries collect: #content as: Array)
		equals: #(42)