tests-insertion
testInsertEmptyLine
	
	self newText: 'AAACCC'.

	cursor linePosition: 3.	
	cursor insert: String cr.
	
	self assert: text asString equals: 'AAA', String cr, 'CCC'.
	
	self assert: cursor lineNumber equals: 2.
	self assert: cursor linePosition equals: 0.
	self assert: text spans size equals: 5.
	