Class {
	#name : #StDebugSessionInspectorNode,
	#superclass : #StInspectorNode,
	#category : #'NewTools-Debugger-Model'
}

{ #category : #accessing }
StDebugSessionInspectorNode >> children [
	^ self hostObject allInspectorNodes
]

{ #category : #accessing }
StDebugSessionInspectorNode >> key [
	^ #'Debug Session'
]

{ #category : #accessing }
StDebugSessionInspectorNode >> rawValue [ 	
	^self hostObject name
]
