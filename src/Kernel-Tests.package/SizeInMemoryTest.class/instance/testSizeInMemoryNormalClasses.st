tests
testSizeInMemoryNormalClasses
	"Two word for the header + one word per instance variable"
	self assert: Date today sizeInMemory = (self align64Bits:( 2 * Smalltalk wordSize + self headerSize)).

	"Two word for the header + one word per instance variable"
	self assert: TestCase new sizeInMemory = (self align64Bits:( 2 * Smalltalk wordSize + self headerSize)).



