tests
testAt
	handle at: 1 write: (ByteArray with: 3) startingAt: 1 count: 1.
	self assert: (handle at: 1) = 3