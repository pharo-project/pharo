tests
testSizeInMemorySmallIntegers
	self assert: SmallInteger minVal sizeInMemory isZero.
	self assert: 0 sizeInMemory isZero.
	self assert: SmallInteger maxVal sizeInMemory isZero.
	self assert: (SmallInteger minVal - 1) sizeInMemory > 0.
	self assert: (SmallInteger maxVal + 1) sizeInMemory > 0