tests - arithmetic
testMultiply
	
	| result |
	self	should: [matrix1 preMultiplyByMatrix: (Matrix new: 3)] raise: Error.

	result := matrix2 preMultiplyByMatrix: matrix1.
	self assert: (result at: 1 at: 1) = 15.
	self assert: (result at: 1 at: 2) = 31.
	self assert: (result at: 2 at: 1) = 22.
	self assert: (result at: 2 at: 2) = 46