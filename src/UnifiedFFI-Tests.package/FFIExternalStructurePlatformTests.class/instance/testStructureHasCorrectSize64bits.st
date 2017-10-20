tests
testStructureHasCorrectSize64bits
	self is64bit ifFalse: [ ^ self skip ].
	
	self 
		assert: (FFIExternalType sizeOf: FFITestStructureByPlatform) 
		equals: 16