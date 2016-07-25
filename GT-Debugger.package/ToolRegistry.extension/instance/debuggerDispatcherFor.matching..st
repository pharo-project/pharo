*GT-Debugger-Model
debuggerDispatcherFor: aContext matching: aBlock
	| debuggers |
	debuggers := self debuggersWithRanksMatching: [ :aDebugger | 
		(aBlock value: aDebugger) and: [ 
			aDebugger handlesContext: aContext ] ].
	(debuggers size > 0) ifTrue: [ ^ (debuggers at: 1) key ].
	^ nil