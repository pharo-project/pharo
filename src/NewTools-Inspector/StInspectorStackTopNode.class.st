"
A model for the `top` stack node. 
This is to be used when debugging.
"
Class {
	#name : #StInspectorStackTopNode,
	#superclass : #StInspectorNode,
	#category : #'NewTools-Inspector-Model'
}

{ #category : #accessing }
StInspectorStackTopNode >> children [
 
	^ #()
]

{ #category : #accessing }
StInspectorStackTopNode >> key [
	^ 'stackTop'
]

{ #category : #accessing }
StInspectorStackTopNode >> rawValue [

	^ self hostObject stackPtr > 0
		ifTrue: [ self hostObject top ]
		ifFalse: [ #emptyStack ]
]
