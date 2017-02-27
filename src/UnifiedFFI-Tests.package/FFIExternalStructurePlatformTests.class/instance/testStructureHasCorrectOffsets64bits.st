tests
testStructureHasCorrectOffsets64bits
	self is64bits ifFalse: [ ^ self skip ].
	
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_LONG) equals: 1.
	self assert: (FFITestStructureByPlatform classPool at: #OFFSET_POINTER) equals: 9