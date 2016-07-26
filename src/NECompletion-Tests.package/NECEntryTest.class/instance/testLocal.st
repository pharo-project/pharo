tests
testLocal
	| entry |
	entry := NECLocalEntry 
		contents: 'abc'
		type: #patternArg:.
	self assert: entry contents = 'abc'.
	self assert: entry type = #patternArg:.
	self assert: entry isLocal