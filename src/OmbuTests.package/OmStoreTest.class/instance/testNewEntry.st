tests
testNewEntry
	anEntry := OmEntry content: 42.
	
	store newEntry: anEntry.
	
	self beforeAndAfterFlushStore: [
		self 
			assert: (store entries collect: #content as: Array)
			equals: #(42) ]