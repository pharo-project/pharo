tests-navigation
testLeftMultiLine

	self newText: 'ABC', String cr, 'DEF'.
	
	cursor moveLeft: 3.
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 0.
	
	cursor moveLeft.
	self assert: cursor lineNumber equals: 1.
	self assert: cursor linePosition equals: 3.