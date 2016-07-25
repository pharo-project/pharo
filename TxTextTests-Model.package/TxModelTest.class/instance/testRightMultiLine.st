tests-navigation
testRightMultiLine
	
	self newText: 'ABC', String cr, 'DEF'.
	
	cursor moveToStart.
	self assert: cursor lineNumber equals: 1.
	self assert: cursor linePosition equals: 0.
	
	cursor moveRight: 3.
	self assert: cursor lineNumber equals: 1.
	self assert: cursor linePosition equals: 3.
	
	cursor moveRight.
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 0.