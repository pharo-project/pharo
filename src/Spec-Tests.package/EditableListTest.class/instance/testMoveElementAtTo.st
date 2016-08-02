tests
testMoveElementAtTo
	testedInstance list: {'AAA' . 'BBB' . 'CCC'}.
	testedInstance moveElementAt: 1 to: 3.
	self
		assert: testedInstance list listItems asArray 
		equals: {'BBB' . 'CCC' . 'AAA'}