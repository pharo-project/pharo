tests - accessing
testTransposed
	| transposedMatrix |
	transposedMatrix := matrix1 transposed.
	self assert: (transposedMatrix at: 1 at: 1) equals: 1.
	self assert: (transposedMatrix at: 1 at: 2) equals: 2.
	self assert: (transposedMatrix at: 2 at: 1) equals: 3.
	self assert: (transposedMatrix at: 2 at: 2) equals: 4