tests-navigation
testEmptyLine
	
	self newText: String cr. 

	self assert: cursor isAtEnd.
	self assert: cursor lineNumber equals: 2.
	
	cursor moveToStart.
		
	self assert: cursor lineNumber equals: 1.
	
	cursor moveRight.
	
	self assert: cursor lineNumber equals: 2.

	cursor moveToStart.
	cursor moveDown.
	
	self assert: cursor lineNumber equals: 2.

