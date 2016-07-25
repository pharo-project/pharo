tests-insertion
testInsertEmptyLineThenMoveRightThenMoveLeftTwice
	
	self newText: 'AAACCC'.

	cursor linePosition: 3.	
	cursor insert: String cr.
	
	self assert: text asString equals: 'AAA', String cr, 'CCC'.
	
	cursor moveRight.
	cursor moveLeft.
	cursor moveLeft.
	self assert: cursor lineNumber equals: 1.
	self assert: cursor linePosition equals: 3.
	