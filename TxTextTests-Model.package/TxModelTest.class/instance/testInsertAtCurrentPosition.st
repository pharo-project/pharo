tests-insertion
testInsertAtCurrentPosition
	
	self newText: 'ABC'.
	
	self assert: text asString equals: 'ABC'.
	
	self assert: cursor isAtEnd.
	self deny: cursor isAtStart.
	
	self assert: cursor linePosition equals: 3.
	self assert: cursor lineNumber equals: 1