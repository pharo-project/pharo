tests
testSuccessfullTestWithHalt

	self 
		assertDebuggerInitializationForContext: [ GTSUnitExampleFailingTest new testWithHalt ] asContext 
		andStepBlock: self stepBlockForTestWithHalt.

	self assert: session testContext method equals: GTSUnitExampleFailingTest >> #testWithHalt.
	self assert: session assertionContext equals: nil.
	self assert: session assertionCallerContext equals: nil.
	
	self deny: session supportsDiff description: 'When there is no assertion no diff is supported'.

 	self assertDebuggerClosing.