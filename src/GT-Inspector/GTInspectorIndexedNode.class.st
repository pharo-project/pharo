"
I am a variable node for representing the indexed variables of an object. I used the index of a variable as a key. I extract the value from the host object every time #rawValue is called.
"
Class {
	#name : #GTInspectorIndexedNode,
	#superclass : #GTInspectorVariableNode,
	#instVars : [
		'index'
	],
	#category : #'GT-Inspector'
}

{ #category : #'instance creation' }
GTInspectorIndexedNode class >> hostObject: anObject index: index [
	^ (self hostObject: anObject)
		index: index;
		yourself
]

{ #category : #accessing }
GTInspectorIndexedNode >> errorMessage [
	^ 'error obtaining indexed attribute value'
]

{ #category : #accessing }
GTInspectorIndexedNode >> index [
	^ index
]

{ #category : #accessing }
GTInspectorIndexedNode >> index: anInteger [
	index := anInteger
]

{ #category : #accessing }
GTInspectorIndexedNode >> key [
	^ self index
]

{ #category : #accessing }
GTInspectorIndexedNode >> rawValue [
	^ self hostObject basicAt: index
]

{ #category : #actions }
GTInspectorIndexedNode >> save: aValue [
	self hostObject at: self index put: aValue
]

{ #category : #accessing }
GTInspectorIndexedNode >> variableTag [
	^ 'attribute'
]
