tests
testSillyStuff

	self 	
		assert: (((1 - 1) * 3) max: -9) equals: 0;
	 	assert: ((2 - 1 - 1) negated) equals: 0;
		assert: ('a' findString: 'a' startingAt: 2 caseSensitive: false) equals: 0.