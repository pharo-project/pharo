tests
testIO
	| out in |
	out := #(1 2 3) asByteArray.
	in := ByteArray new: 3.
	handle at: 1 write: out startingAt: 1 count: 3.
	handle at: 1 read: in startingAt: 1 count: 3.
	self assert: out = in.