tests
testNextInt
	| int |
	int := gen nextInt: 256.
	self assert: int isInteger.
	self assert: (int between: 1 and: 256)