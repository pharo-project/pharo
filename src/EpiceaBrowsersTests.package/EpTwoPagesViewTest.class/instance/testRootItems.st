tests
testRootItems
	self 
		assert: view rootItems size 
		equals: self pageSize + 1 "-> 1 extra for the pagination item"