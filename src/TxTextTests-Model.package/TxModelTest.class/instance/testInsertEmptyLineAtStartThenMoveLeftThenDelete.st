tests-insertion
testInsertEmptyLineAtStartThenMoveLeftThenDelete
	
	self newText: 'ABC'.
	
	cursor moveToStart. 
	cursor insert: String cr.
	cursor moveLeft.
	selection := cursor asSelection.
	selection deleteKey.
	
	cursor := selection end.
	
	self assert: cursor isAtStart.
	
	self assert: text asString equals: 'ABC'.
	self assert: cursor linePosition equals: 0
