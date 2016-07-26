tests
testDebugger

	self 
		assertDebuggerInitializationForContext: [ 20 factorial ] asContext 
		andStepBlock: [:aProcess | "The bytecode debugger can handle any context" ].

 	self assertDebuggerClosing.