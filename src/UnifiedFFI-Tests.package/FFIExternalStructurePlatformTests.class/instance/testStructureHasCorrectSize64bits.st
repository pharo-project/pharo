tests
testStructureHasCorrectSize64bits
	self is64bits ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 16