tests-insertion
testInsertEmptyLineAtEndThenMoveLeftThenDelete
	
	self newText: 'ABC'.
	
	cursor moveToEnd. 
	cursor insert: String cr.
	
	cursor moveLeft.
	selection := cursor asSelection.
	
	selection deleteKey.
	
	self assert: selection end isAtEnd.
	
	self assert: text asString equals: 'ABC'.
	self assert: selection end linePosition equals: 3
