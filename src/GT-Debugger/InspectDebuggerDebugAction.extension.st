Extension { #name : #InspectDebuggerDebugAction }

{ #category : #'*GT-Debugger' }
InspectDebuggerDebugAction class >> gtActionFor: aDebugger [
	<gtStackDebuggingAction>

	^ (self forDebugger: aDebugger)
			order: 60
]
