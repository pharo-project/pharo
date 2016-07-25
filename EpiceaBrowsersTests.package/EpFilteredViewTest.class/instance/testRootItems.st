tests
testRootItems
	self assert: view rootItems size equals: 3.
	
	self assert: (self andFilter accepts: view rootItems second entry)