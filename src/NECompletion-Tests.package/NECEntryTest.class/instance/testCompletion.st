tests
testCompletion
	| entry |
	entry := NECSelectorEntry 
		contents: 'makeItHappen'
		type: #unary:.
	self assert: entry completion = #makeItHappen