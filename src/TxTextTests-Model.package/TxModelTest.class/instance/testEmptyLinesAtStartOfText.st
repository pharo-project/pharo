tests-navigation
testEmptyLinesAtStartOfText
	
	self newText: String cr, String cr, String cr, 'CDE'. 
	
	self assert: cursor isAtEnd.
	self assert: cursor lineNumber equals: 4