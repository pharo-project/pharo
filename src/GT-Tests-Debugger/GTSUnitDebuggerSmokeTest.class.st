Class {
	#name : #GTSUnitDebuggerSmokeTest,
	#superclass : #GTDebuggerSmokeTest,
	#category : #'GT-Tests-Debugger'
}

{ #category : #accessing }
GTSUnitDebuggerSmokeTest >> debuggerToTest [

	^ GTSUnitDebugger
]

{ #category : #utils }
GTSUnitDebuggerSmokeTest >> stepBlockForTestArrays [

	^ [ :aProcess |
		aProcess
			"Step into the execution of the test method"
			completeStep: aProcess suspendedContext;
			completeStep: aProcess suspendedContext;	
			step;
			
			"Step into the assert method"
			completeStep: aProcess suspendedContext; 
			completeStep: aProcess suspendedContext;
			completeStep: aProcess suspendedContext;
			step ]
		

]

{ #category : #utils }
GTSUnitDebuggerSmokeTest >> stepBlockForTestWithHalt [

	^ [ :aProcess |
		aProcess 
			"Step into the execution of the test method"
			completeStep: aProcess suspendedContext;
			completeStep: aProcess suspendedContext;	
			step;
		
			"Step until before the message send #halt"
			stepToSendOrReturn;
			completeStep: aProcess suspendedContext;
			stepToSendOrReturn ]
		

]

{ #category : #tests }
GTSUnitDebuggerSmokeTest >> testAssertionEqualsDetected [

	self 
		assertDebuggerInitializationForContext: [ GTSUnitExampleFailingTest new testArrays ] asContext 
		andStepBlock: self stepBlockForTestArrays.

	self assert: session testContext method equals: GTSUnitExampleFailingTest >> #testArrays.
	self assert: session assertionContext method selector equals: #assert:equals:.
	self assert: session assertionCallerContext equals: session testContext.
	
	self assert: session supportsDiff description: 'When #assert:equals: is encountered a diff can be used'.

 	self assertDebuggerClosing.
]

{ #category : #tests }
GTSUnitDebuggerSmokeTest >> testSuccessfullTestWithHalt [

	self 
		assertDebuggerInitializationForContext: [ GTSUnitExampleFailingTest new testWithHalt ] asContext 
		andStepBlock: self stepBlockForTestWithHalt.

	self assert: session testContext method equals: GTSUnitExampleFailingTest >> #testWithHalt.
	self assert: session assertionContext equals: nil.
	self assert: session assertionCallerContext equals: nil.
	
	self deny: session supportsDiff description: 'When there is no assertion no diff is supported'.

 	self assertDebuggerClosing.
]
