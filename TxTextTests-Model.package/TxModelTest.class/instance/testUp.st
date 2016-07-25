tests-navigation
testUp
	| pos |
	self newText: 'ABC', String cr, 'DEF'.
	
	pos := text endPosition moveUp.
	
	self assert: pos lineNumber equals: 1.
	self assert: pos linePosition equals: 3.
	
	pos := pos moveUp.
	self assert: pos isAtStart.