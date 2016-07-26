tests
testSizeAfterGrow
	| out |
	out := #(1 2 3) asByteArray.
	handle at: 1 write: out startingAt: 1 count: 3.
	self assert: handle size = 3