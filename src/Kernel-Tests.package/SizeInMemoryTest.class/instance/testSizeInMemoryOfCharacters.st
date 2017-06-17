tests
testSizeInMemoryOfCharacters
	self assert: 0 asCharacter sizeInMemory isZero.
	self assert: 255 asCharacter sizeInMemory isZero.
	self assert: 256 asCharacter sizeInMemory isZero.
	self assert: 16r10FFFF asCharacter sizeInMemory isZero