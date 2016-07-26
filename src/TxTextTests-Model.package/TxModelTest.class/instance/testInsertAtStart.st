tests-insertion
testInsertAtStart
	
	self newText: 'ABC'.
	
	cursor moveToStart.
	
	self assert: cursor isAtStart.
	cursor insert: 'xyz'.
	
	self assert: text asString equals: 'xyzABC'.	
	self assert: cursor linePosition equals: 3
