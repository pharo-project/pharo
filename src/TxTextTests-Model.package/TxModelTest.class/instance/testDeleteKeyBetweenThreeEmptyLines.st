tests-navigation
testDeleteKeyBetweenThreeEmptyLines

	self newText: 'ABC', String cr, String cr, String cr, 'DEF'.
	
	cursor moveToStart; moveToLineEnd; moveRight.
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 0.
	
	selection := cursor asSelection.
	selection deleteKey.
	
	self assert: text asString equals: 'ABC

DEF'.