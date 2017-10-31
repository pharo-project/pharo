some tests
testAsSet

	| newFull |
	newFull := #(1 2 3) asSet.
	newFull add: 4.
	self assert: newFull equals: (Set with: 1 with: 2 with: 3 with: 4)