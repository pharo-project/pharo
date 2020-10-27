"
I am a variable node for representing the indexed variables of an object. 
I used the index of a variable as a key. 
I extract the value from the host object every time #rawValue is called
"
Class {
	#name : #StInspectorIndexedNode,
	#superclass : #StInspectorNode,
	#instVars : [
		'index'
	],
	#category : #'NewTools-Inspector-Model'
}

{ #category : #'instance creation' }
StInspectorIndexedNode class >> hostObject: anObject index: index [
	^ (self hostObject: anObject)
		index: index;
		yourself
]

{ #category : #comparing }
StInspectorIndexedNode >> = anObject [

	^ super = anObject and: [ index = anObject index ]
]

{ #category : #'as yet unclassified' }
StInspectorIndexedNode >> debuggerColor [
	^(SHRBTextStyler new attributesFor: #instVar) first color
]

{ #category : #accessing }
StInspectorIndexedNode >> errorMessage [
	^ 'error obtaining indexed attribute value'
]

{ #category : #comparing }
StInspectorIndexedNode >> hash [

	^ index hash
]

{ #category : #accessing }
StInspectorIndexedNode >> index [
	^ index
]

{ #category : #accessing }
StInspectorIndexedNode >> index: anInteger [
	index := anInteger
]

{ #category : #accessing }
StInspectorIndexedNode >> key [
	^ self index
]

{ #category : #accessing }
StInspectorIndexedNode >> rawValue [

	^ self hostObject basicAt: index
]

{ #category : #actions }
StInspectorIndexedNode >> save: aValue [
	self hostObject at: self index put: aValue
]

{ #category : #accessing }
StInspectorIndexedNode >> variableTag [
	^ 'attribute'
]
