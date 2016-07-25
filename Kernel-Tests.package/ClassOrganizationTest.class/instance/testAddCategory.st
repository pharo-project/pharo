tests
testAddCategory
	
	self organization addCategory: 'test-protocol'.
	
	self assert: (self organization categories includes: 'test-protocol')