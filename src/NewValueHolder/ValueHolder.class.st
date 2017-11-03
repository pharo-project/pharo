"
A value holder holding a value 
"
Class {
	#name : #ValueHolder,
	#superclass : #Model,
	#instVars : [
		'contents'
	],
	#category : #NewValueHolder
}

{ #category : #comparing }
ValueHolder >> = another [

	self species = another species ifFalse: [ ^ false ].
	^ self contents = another contents
]

{ #category : #accessing }
ValueHolder >> contents [
	^contents
]

{ #category : #accessing }
ValueHolder >> contents: newContents [
	contents := newContents.
	self changed: #contents
]
