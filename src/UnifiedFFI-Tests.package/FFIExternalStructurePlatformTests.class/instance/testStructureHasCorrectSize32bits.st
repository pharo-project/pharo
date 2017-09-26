tests
testStructureHasCorrectSize32bits
	self is32bit ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 8