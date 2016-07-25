tests-navigation
testRightSingleLine

	self newText: 'ABC'.
		
	cursor moveToStart.
	self assert: cursor isAtStart.
	self assert: cursor linePosition equals: 0.
	
	cursor moveRight: 3.
	self assert: cursor isAtEnd.
	self assert: cursor linePosition equals: 3.
	
	cursor moveRight.
	self assert: cursor isAtEnd.
	self assert: cursor linePosition equals: 3.