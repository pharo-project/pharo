tests-insertion
testInsertMultilineString
	
	self newText: 'AAACCC'.

	cursor linePosition: 3.	
	cursor insert: 'bb', String cr, 'bb'.
	
	self assert: text asString equals: 'AAAbb', String cr, 'bbCCC'.
	
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 2