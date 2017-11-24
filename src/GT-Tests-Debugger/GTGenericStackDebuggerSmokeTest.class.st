Class {
	#name : #GTGenericStackDebuggerSmokeTest,
	#superclass : #GTDebuggerSmokeTest,
	#category : #'GT-Tests-Debugger'
}

{ #category : #accessing }
GTGenericStackDebuggerSmokeTest >> debuggerToTest [

	^ GTGenericStackDebugger
]

{ #category : #tests }
GTGenericStackDebuggerSmokeTest >> testDebugger [

	self 
		assertDebuggerInitializationForContext: [ 20 factorial ] asContext 
		andStepBlock: [:aProcess | "The generic debugger can handle any context" ].

 	self assertDebuggerClosing.
]
