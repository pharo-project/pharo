tests-navigation
testDeleteKeyBeforeTwoLines

	self newText: 'ABC', String cr, String cr, 'DEF'.
	
	cursor moveToStart.
	cursor linePosition: 3.
	
	selection := cursor asSelection.
	
	selection deleteKey.
	selection deleteKey.
	
	self assert: text asString equals: 'ABCDEF'.
	
	self assert: cursor lineNumber equals: 1.
	self assert: cursor linePosition equals: 3