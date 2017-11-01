Extension { #name : #Process }

{ #category : #'*DebuggerModel' }
Process >> newDebugSessionNamed: aString startedAt: aContext [

	^DebugSession named: aString on: self startedAt: aContext
]
