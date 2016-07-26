tests
testExpand
	view rootItems last fetchMore.

	self 
		assert: view rootItems size
		equals:  browser log entries size. " -> All the entries are displayed"