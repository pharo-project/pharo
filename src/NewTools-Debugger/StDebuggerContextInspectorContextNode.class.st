"
I retrieve context nodes from a StDebuggerContext object.
My children are extracted from a StDebugger context.
"
Class {
	#name : #StDebuggerContextInspectorContextNode,
	#superclass : #StDebuggerContextInspectorNode,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerContextInspectorContextNode >> computeChildrenFromSourceObject [
	"(StNodeCollector for: hostObject) collectContextNodes"
	^ {}
]

{ #category : #accessing }
StDebuggerContextInspectorContextNode >> label [
	^'thisContext'
]

{ #category : #accessing }
StDebuggerContextInspectorContextNode >> rawValue [
	^ hostObject context
]
