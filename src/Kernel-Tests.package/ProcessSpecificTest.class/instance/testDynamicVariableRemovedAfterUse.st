testing
testDynamicVariableRemovedAfterUse
	TestDynamicVariable
		value: 3
		during: [  ].
		
	self assert: (Processor activeProcess psValueAt: TestDynamicVariable soleInstance index) equals: nil