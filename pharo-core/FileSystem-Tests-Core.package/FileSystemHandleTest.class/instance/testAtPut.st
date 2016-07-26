tests
testAtPut
	| in |
	handle at: 1 put: 3.
	in := ByteArray new: 1.
	handle at: 1 read: in startingAt: 1 count: 1.
	self assert: in first = 3