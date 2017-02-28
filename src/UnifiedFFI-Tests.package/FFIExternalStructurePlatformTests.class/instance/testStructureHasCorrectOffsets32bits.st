tests
testStructureHasCorrectOffsets32bits
	self is32bits ifFalse: [ ^ self skip ].
	
	FFITestStructureByPlatform compiledSpec. "Ensure fields are initialized"	
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_LONG) equals: 1.
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_POINTER) equals: 5