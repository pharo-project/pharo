tests-insertion
testInsertTwoEmptyLinesThenDeleteFirstAndMoveRight
	
	self newText: 'AAACCC'.

	cursor linePosition: 3.	
	cursor insert: String cr; insert: String cr.
	cursor moveToStart.
	cursor linePosition: 3.	
		
	selection := cursor asSelection. 
	selection deleteKey.
	cursor moveRight.
	
	self assert: text asString equals: 'AAA', String cr, 'CCC'.
	
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 0.
	