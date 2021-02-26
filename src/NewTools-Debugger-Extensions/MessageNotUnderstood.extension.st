Extension { #name : #MessageNotUnderstood }

{ #category : #'*NewTools-Debugger-Extensions' }
MessageNotUnderstood >> stDebuggerInspectorNodes [

	| nodes |
	nodes := super stDebuggerInspectorNodes.
	nodes add: (StInspectorDynamicNode
			 hostObject: self receiver
			 label: 'DNU receiver'
			 value: self receiver).
	^ nodes
]
