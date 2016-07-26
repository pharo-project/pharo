tests
testByteArrayLiteral
	self class compile: 'array ^ #[ 1 2 3 4 ]'.
	self assert: (self array = self array).
	self assert: (self array == self array)