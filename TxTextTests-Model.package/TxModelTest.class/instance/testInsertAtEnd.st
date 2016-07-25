tests-insertion
testInsertAtEnd
	
	self newText: 'ABC'.
	
	cursor moveToEnd. 
	
	self assert: cursor isAtEnd.
	
	cursor insert: 'xyz'.
	
	self assert: text asString equals: 'ABCxyz'.
	self assert: cursor linePosition equals: 6
