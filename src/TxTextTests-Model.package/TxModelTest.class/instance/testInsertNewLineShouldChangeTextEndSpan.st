tests-insertion
testInsertNewLineShouldChangeTextEndSpan
	
	self newText: 'AAACCC'.

	cursor linePosition: 3.	
	cursor insert: String cr.
	
	self assert: text asString = ('AAA',String cr,'CCC').
	self assert: cursor lineNumber equals: 2.