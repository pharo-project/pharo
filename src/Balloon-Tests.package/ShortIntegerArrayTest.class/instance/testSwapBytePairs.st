tests
testSwapBytePairs

	shortIntegerArray swapBytePairsHacky.
	self assert: 4*256 equals: (shortIntegerArray at:1). 
	self assert: 4*256 + 1 equals: (shortIntegerArray at:2).
	self assert: 2*256 equals: (shortIntegerArray at:3).
	self assert: 1*256 + 10 equals: (shortIntegerArray at:4).
