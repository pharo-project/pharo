tests
testPrintOn
	| entry |
	entry := NECSelectorEntry 
		contents: 'compute'
		type: #unary:.
	self assert: 'NECSelectorEntry(compute,unary:)' = entry printString