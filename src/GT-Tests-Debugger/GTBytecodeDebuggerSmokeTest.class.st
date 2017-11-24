Class {
	#name : #GTBytecodeDebuggerSmokeTest,
	#superclass : #GTDebuggerSmokeTest,
	#category : #'GT-Tests-Debugger'
}

{ #category : #accessing }
GTBytecodeDebuggerSmokeTest >> debuggerToTest [

	^ GTBytecodeDebugger
]

{ #category : #tests }
GTBytecodeDebuggerSmokeTest >> testDebugger [

	self 
		assertDebuggerInitializationForContext: [ 20 factorial ] asContext 
		andStepBlock: [:aProcess | "The bytecode debugger can handle any context" ].

 	self assertDebuggerClosing.
]
