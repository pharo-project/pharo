tests
testReadBufferTooLarge
	| out in result |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 5.
	in atAllPut: 9.
	handle at: 1 write: out startingAt: 1 count: 3.
	result := handle at: 1 read: in startingAt: 2 count: 4.
	self assert: result = 3.
	self assert: in = #(9 1 2 3 9) asByteArray.