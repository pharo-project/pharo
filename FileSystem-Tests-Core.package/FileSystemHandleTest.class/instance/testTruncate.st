tests
testTruncate
	| out |
	out := #(1 2 3 4 5) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 5.
	handle truncateTo: 3.
	self assert: handle size = 3