tests-navigation
testLinePosition
	
	self newText: 'ABC'.
	
	self assert: cursor isAtEnd.
	
	cursor linePosition: 1.
	self deny: cursor isAtStart.
	self assert: cursor linePosition equals: 1.
	self deny: cursor isAtEnd.
	
	cursor linePosition: 0.
	self assert: cursor isAtStart.
	self assert: cursor linePosition equals: 0.
	self deny: cursor isAtEnd.
	