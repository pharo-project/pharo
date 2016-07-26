tests-navigation
testEmptyLines
	
	self newText: String cr, String cr, String cr. 
	
	self assert: cursor isAtEnd.	
	self assert: cursor lineNumber equals: 4.
	
