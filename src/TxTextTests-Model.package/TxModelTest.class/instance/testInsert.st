tests-insertion
testInsert
	
	self newText: 'AAACCC'.
	
	self assert: cursor linePosition equals: 6.
	
	cursor linePosition: 3.
	self assert: cursor linePosition equals: 3.
	
	cursor insert: 'bbb'.
	
	self assert: text asString equals: 'AAAbbbCCC'.
	
	self assert: cursor linePosition equals: 6
