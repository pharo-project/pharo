tests
testAssertionEqualsDetected

	self 
		assertDebuggerInitializationForContext: [ GTSUnitExampleFailingTest new testArrays ] asContext 
		andStepBlock: self stepBlockForTestArrays.

	self assert: session testContext method equals: GTSUnitExampleFailingTest >> #testArrays.
	self assert: session assertionContext method selector equals: #assert:equals:.
	self assert: session assertionCallerContext equals: session testContext.
	
	self assert: session supportsDiff description: 'When #assert:equals: is encountered a diff can be used'.

 	self assertDebuggerClosing.