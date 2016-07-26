*GT-Debugger-Model
registeredDebuggersWithRanks
	
	^ tools at: #debuggers ifAbsent: [
		tools at: #debuggers put: OrderedCollection new ]