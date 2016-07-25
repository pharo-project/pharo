tests
testSizeInMemoryOfByteObjects
	"Byte objects should be padded to words"
	self assert: 'a' sizeInMemory equals: 12.
	self assert: 'abcd' sizeInMemory equals: 12.
	self assert: '' sizeInMemory equals: 8.
	self assert: 'abcde' sizeInMemory equals: 16.
	self assert: 'abcdefghi' sizeInMemory equals: 20.