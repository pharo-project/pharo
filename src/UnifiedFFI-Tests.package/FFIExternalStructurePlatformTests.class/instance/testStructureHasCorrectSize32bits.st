tests
testStructureHasCorrectSize32bits
	self is32bits ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 8