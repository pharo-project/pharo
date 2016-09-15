tests
testFirstEntryIfAbsent
	self 
		assert: (log firstEntryIfAbsent: [ self fail ]) 
		equals: log entries first