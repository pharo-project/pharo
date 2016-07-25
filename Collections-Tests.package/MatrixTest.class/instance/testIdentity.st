tests - instance creation
testIdentity

	| m  m2 | 
	m := Matrix new: 3 element: 0.
	m at: 1 at:1 put: 1.
	m at: 2 at:2 put: 1.
	m at: 3 at:3 put: 1.
	self assert: (m occurrencesOf: 1) equals: 3.
	m2 := Matrix identity: 3.
	self assert: (m2 = m).
