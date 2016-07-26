tests
testDebugger

	self 
		assertDebuggerInitializationForContext: [ 20 factorial ] asContext 
		andStepBlock: [:aProcess | "The generic debugger can handle any context" ].

 	self assertDebuggerClosing.