tests - logical operations
testXor
	self assert: (false xor: true) = true.
	self assert: (false xor: false) = false.
	self assert: (false xor: [true]) = true.
	self assert: (false xor: [false]) = false.