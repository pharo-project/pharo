Extension { #name : #FuelOutStackDebugAction }

{ #category : #'*GT-Debugger-Actions' }
FuelOutStackDebugAction class >> gtActionFor: aDebugger [
	<gtStackDebuggingAction>
	
	^ (self forDebugger: aDebugger)
			order: 30
]
