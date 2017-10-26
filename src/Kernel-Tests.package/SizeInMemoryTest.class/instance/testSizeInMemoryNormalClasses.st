tests
"protocol: tests"
testSizeInMemoryNormalClasses
	"Two word for the header + one word per instance variable"
	self assert: Date today sizeInMemory = ( (2+2) * Smalltalk wordSize ).

	"Two word for the header + one word per instance variable"
	self assert: TestCase new sizeInMemory = ( (2+2) * Smalltalk wordSize ).



