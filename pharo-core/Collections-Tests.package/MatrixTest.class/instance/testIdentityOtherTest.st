tests - instance creation
testIdentityOtherTest

	| m | 
	m := Matrix identity: 3.
	self assert: (m at: 1 at:1) equals: 1.
	self assert: (m at: 2 at:2) equals: 1.
	self assert: (m at: 3 at:3) equals: 1.
	self assert: (m occurrencesOf: 1) equals: 3.
