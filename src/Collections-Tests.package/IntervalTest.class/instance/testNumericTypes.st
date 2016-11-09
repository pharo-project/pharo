tests
testNumericTypes

	self assert: ((3 asNumber) to: 5) equals: #(3 4 5).
	
	self assert: (3.0 to: 5.0) equals: #(3.0 4.0 5.0).
	self assert: (3.0 to: 5.0 by: 0.5) equals: #(3.0 3.5 4.0 4.5 5.0).
	
	self assert: (3/1 to: 5/1) equals: #(3 4 5).

	"dynamic array, because fractions aren't literals"
	self assert: (1/2 to: 5/2 by: 1/2) equals: {1/2 . 1 . 3/2 . 2 . 5/2}.