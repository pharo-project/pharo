tests
testSizeInMemorySmallIntegers
	self assert: 42 sizeInMemory= 0.
	self assert: -42 sizeInMemory= 0.
	self assert: 0 sizeInMemory= 0.
	self assert: 999999999999999999999 sizeInMemory > 0.

	