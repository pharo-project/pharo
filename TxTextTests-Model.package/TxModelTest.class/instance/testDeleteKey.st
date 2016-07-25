tests-navigation
testDeleteKey

	self newText: 'ABC',String cr,'DEF'.
	
	cursor moveToStart moveToLineEnd.

	selection 
		start: cursor;
		end: cursor.
		
	selection deleteKey.
	
	self assert: text asString equals: 'ABCDEF'.
	
	selection deleteKey.
	self assert: text asString equals: 'ABCEF'