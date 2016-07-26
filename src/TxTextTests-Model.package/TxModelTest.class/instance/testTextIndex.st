tests-navigation
testTextIndex
	| pos |
	self newText: 'ABC', String cr, 'DEF'.
	
	pos := text startPosition moveRight: 4.
	
	self assert: pos asTextIndex equals: 4.
	
	pos := text positionAtTextIndex: 4.
	self assert: pos linePosition equals: 0.
	self assert: pos isAtLineStart.
	
	pos := text positionAtTextIndex: 3.

	self assert: pos linePosition equals: 3.
	self assert: pos isAtLineEnd.
