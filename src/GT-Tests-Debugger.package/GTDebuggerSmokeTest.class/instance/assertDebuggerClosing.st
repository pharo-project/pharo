asserting
assertDebuggerClosing
	
 	debugger close.

	self assert: window model equals: nil.
	self assert: debugger session interruptedContext equals: nil.
	self assert: debugger session interruptedProcess equals: nil.