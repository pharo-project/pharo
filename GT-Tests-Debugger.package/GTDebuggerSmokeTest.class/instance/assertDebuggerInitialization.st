asserting
assertDebuggerInitialization
	
	self assert: (debugger class handlesContext: session interruptedContext).
	
	window := debugger openOn: session.
	
	self assert: window model equals: debugger.
	self assert: debugger session interruptedContext notNil.
	self assert: debugger session interruptedProcess notNil.
	