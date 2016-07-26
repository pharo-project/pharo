tests-navigation
testMoveToLineEnd

	self newText: 'ac', String cr, 'def', String cr, 'ghi'.
	
	cursor moveToStart.
	cursor moveDown.
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 0.
	
	cursor moveToLineEnd.
	
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 3.