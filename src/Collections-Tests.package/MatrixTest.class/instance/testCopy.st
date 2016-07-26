tests - copying
testCopy

	| copyMatrix |
	copyMatrix := matrix1 copy.
	self assert: matrix1 = copyMatrix 