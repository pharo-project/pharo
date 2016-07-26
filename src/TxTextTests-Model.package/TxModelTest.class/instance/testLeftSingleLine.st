tests-navigation
testLeftSingleLine
	
	self newText: 'ABC'.
	
	self deny: cursor isAtStart.
	
	cursor moveLeft: 3.
	self assert: cursor isAtStart.
	
	cursor moveLeft.
	self assert: cursor isAtStart.