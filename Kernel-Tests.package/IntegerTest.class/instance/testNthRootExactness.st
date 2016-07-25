tests - mathematical functions
testNthRootExactness
	| inexactRoots largeExactPowersOf6 |
	largeExactPowersOf6 := (2 to: 100) collect: [:k | k raisedTo: 66].
	inexactRoots := largeExactPowersOf6 reject: [:e | (e nthRoot: 6) isInteger].
	self assert: inexactRoots isEmpty description: 'Failed to find the exact 6th root of these numbers'