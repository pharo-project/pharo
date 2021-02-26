Extension { #name : #Exception }

{ #category : #'*NewTools-Debugger-Extensions' }
Exception >> signalContext [

	^ signalContext
]

{ #category : #'*NewTools-Debugger-Extensions' }
Exception >> stDebuggerInspectorNodes [

	| nodes |
	nodes := OrderedCollection new.
	nodes add: (StInspectorDynamicNode
			 hostObject: self
			 label: 'Exception'
			 value: self).
	^ nodes
]
