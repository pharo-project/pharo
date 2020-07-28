"
I retrieve receiver nodes from a StDebuggerContext object.
My children are extracted from a StDebugger context's receiver.
"
Class {
	#name : #StDebuggerContextInspectorReceiverNode,
	#superclass : #StDebuggerContextInspectorNode,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerContextInspectorReceiverNode >> computeChildrenFromSourceObject [
	^ hostObject context home receiver allInspectorNodes
		reject: [ :node | node key = 'self' ]
]

{ #category : #accessing }
StDebuggerContextInspectorReceiverNode >> label [
	^ 'self'" asText makeAllColor: self colorForSelfNode"
]

{ #category : #accessing }
StDebuggerContextInspectorReceiverNode >> rawValue [
	^ hostObject context home receiver
]

{ #category : #accessing }
StDebuggerContextInspectorReceiverNode >> stringValue [
	^self rawValue printString" asText makeAllColor: self colorForSelfNode"
]
