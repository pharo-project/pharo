tests-navigation
testDown

	self newText: 'ABC', String cr, 'DEF'.

	cursor moveToStart.	
	cursor moveDown.

	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 0.
	
	cursor moveDown.
	self assert: cursor isAtEnd.