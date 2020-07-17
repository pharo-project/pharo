"
I represent a node in the debugger inspector list, with special computation for retrieving my children and my key (= displayed label).
"
Class {
	#name : #StDebuggerContextInspectorNode,
	#superclass : #StInspectorNode,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebuggerContextInspectorNode >> children [ 
	^self computeChildrenFromSourceObject
]

{ #category : #accessing }
StDebuggerContextInspectorNode >> computeChildrenFromSourceObject [
	^self subclassResponsibility 
]

{ #category : #accessing }
StDebuggerContextInspectorNode >> key [
	^self label
]
