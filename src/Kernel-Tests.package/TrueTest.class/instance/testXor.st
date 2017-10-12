tests - logical operations
testXor
	self assert: (true xor: true) = false.
	self assert: (true xor: false) = true.
	self assert: (true xor: [true]) = false.
	self assert: (true xor: [false]) = true.