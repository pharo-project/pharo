tests-navigation
testStartEnd
	| pos |
	self newText: 'ABC', String cr, 'DEF'.
	
	pos := text startPosition.
	
	self assert: pos isAtStart.
	self assert: pos lineNumber equals: 1.
	self assert: pos linePosition equals: 0.
	
	pos := text endPosition.

	self assert: pos isAtEnd.
	self assert: pos lineNumber equals: 2.
	self assert: pos linePosition equals: 3.