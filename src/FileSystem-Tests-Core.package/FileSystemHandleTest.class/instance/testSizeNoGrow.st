tests
testSizeNoGrow
	| bytes |
	bytes := #(1 2 3 4) asByteArray.
	handle at: 1 write: bytes startingAt: 1 count: 3.
	handle at: 4 write: bytes startingAt: 4 count: 1.
	self assert: handle size = 4