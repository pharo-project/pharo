Extension { #name : #Process }

{ #category : #'*Debugger-Model' }
Process >> newDebugSessionNamed: aString startedAt: aContext [

	^DebugSession named: aString on: self startedAt: aContext
]
