tests
testSizeInMemoryNormalClasses
	"Two word for the header + one word per instance variable"
	self assert: Date today sizeInMemory = 16.

	"Two word for the header + one word per instance variable"
	self assert: TestCase new sizeInMemory = 16.



