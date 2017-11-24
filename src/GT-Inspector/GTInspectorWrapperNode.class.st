"
I am a node that wraps in the host object another node and delegates all API methods to the wrapped node. I can further provide a different label for the wrapped node.
"
Class {
	#name : #GTInspectorWrapperNode,
	#superclass : #GTInspectorVariableNode,
	#instVars : [
		'label'
	],
	#category : #'GT-Inspector'
}

{ #category : #'instance creation' }
GTInspectorWrapperNode class >> hostObject: anObject label: aString [
	^ (self hostObject: anObject) 
		label: aString;
		yourself
]

{ #category : #composition }
GTInspectorWrapperNode >> childrenNodesFor: aVariablesBrowser [
	^ self hostObject childrenNodesFor: aVariablesBrowser
]

{ #category : #accessing }
GTInspectorWrapperNode >> errorMessage [
	^ self hostObject errorMessage
]

{ #category : #accessing }
GTInspectorWrapperNode >> inspectorActionsForNode [
	^ self hostObject inspectorActionsForNode
]

{ #category : #accessing }
GTInspectorWrapperNode >> key [
	^ label ifNil: [ self hostObject key ]
]

{ #category : #accessing }
GTInspectorWrapperNode >> label: aString [
	label := aString
]

{ #category : #accessing }
GTInspectorWrapperNode >> rawValue [
	^ self hostObject rawValue
]

{ #category : #accessing }
GTInspectorWrapperNode >> variableTag [
	^ self hostObject variableTag
]
