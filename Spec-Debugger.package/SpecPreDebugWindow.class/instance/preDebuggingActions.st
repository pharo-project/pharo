actions lookup
preDebuggingActions
	"Return a collection of debug actions constructed based on the pragmas 'self preDebuggingActionsPragmas'. 
	These actions should understand the message #preDebugWindow:"
	
	^ (self actionsForPragmas: self preDebuggingActionsPragmas) 
		do: [ :each | 
			each preDebugWindow: self ]