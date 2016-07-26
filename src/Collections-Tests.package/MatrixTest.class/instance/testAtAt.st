tests - accessing
testAtAt
	self should:[matrix1 at: 2 at: 3] raise: Error.
	self should:[matrix1 at: 3 at: 2] raise: Error.
	self should:[matrix1 at: 3 at: 3] raise: Error.
	self should:[matrix1 at: 0 at: 1] raise: Error.
	self should:[matrix1 at: 1 at: 0] raise: Error.
	self should:[matrix1 at: 0 at: 0] raise: Error.
	
	self assert: (matrix1 at: 1 at: 1) equals: 1.
	self assert: (matrix23 at: 1 at: 1) equals: 11.
	self assert: (matrix23 at: 1 at: 2) equals: 21.
	self assert: (matrix23 at: 2 at: 1) equals: 12.
	self assert: (matrix23 at: 2 at: 2) equals: 22.
	self assert: (matrix23 at: 3 at: 1) equals: 13.
	self assert: (matrix23 at: 3 at: 2) equals: 23.