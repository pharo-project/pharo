"
I model the  `self` variable. This the value of this variable is always the host object. 
"
Class {
	#name : #GTInspectorSelfNode,
	#superclass : #GTInspectorVariableNode,
	#category : #'GT-Inspector'
}

{ #category : #composition }
GTInspectorSelfNode >> childrenNodesFor: aVariablesBrowser [
	"Self node does not have any subvariables, just return empty collection"
	<return: #Collection of: #GTInspectorVariableNode>

	^ #()
]

{ #category : #accessing }
GTInspectorSelfNode >> key [
	^ 'self'
]

{ #category : #accessing }
GTInspectorSelfNode >> rawValue [
	^ self hostObject
]
