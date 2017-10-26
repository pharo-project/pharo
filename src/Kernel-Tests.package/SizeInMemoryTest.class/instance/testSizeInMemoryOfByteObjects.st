tests
testSizeInMemoryOfByteObjects
	"Byte objects should be padded to words"
	self assert: 'a' sizeInMemory equals: (self paddedByteStringSize: 1).
	self assert: 'abcd' sizeInMemory equals: (self paddedByteStringSize: 4).
	self assert: '' sizeInMemory equals: (self paddedByteStringSize: 0).
	self assert: 'abcde' sizeInMemory equals: (self paddedByteStringSize: 5).
	self assert: 'abcdefghi' sizeInMemory equals: (self paddedByteStringSize: 9).