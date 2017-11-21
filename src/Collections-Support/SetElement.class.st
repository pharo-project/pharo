"
I am a helper class for handling the contents of Set.
My main purpose is to allow a Set to contain nil.
Since nil represents an empty slot in a Set, it cannot be stored directly in the array of values. Instead, an instance of SetElement is used as a wrapper. See implementors and senders of #asElement for wrapping, and #enclosedSetElement for unwrapping.
"
Class {
	#name : #SetElement,
	#superclass : #Object,
	#instVars : [
		'enclosedElement'
	],
	#classVars : [
		'NilElement'
	],
	#category : #Collections-Support
}

{ #category : #'class initialization' }
SetElement class >> initialize [
	NilElement := self with: nil.

]

{ #category : #'instance creation' }
SetElement class >> with: anObject [
	^ self new enclosedSetElement: anObject
]

{ #category : #accessing }
SetElement class >> withNil [
	^ NilElement

]

{ #category : #comparing }
SetElement >> = anObject [
	^ anObject class = self class and: [ enclosedElement = anObject enclosedSetElement ]

]

{ #category : #converting }
SetElement >> asSetElement [
	"A receiver has to be included into a set, as a distinct object.
	We need to wrap receiver in own turn, otherwise #enclosedSetElement will return wrong object for that set"
	^ SetElement with: self
]

{ #category : #accessing }
SetElement >> enclosedSetElement [
	^ enclosedElement
]

{ #category : #accessing }
SetElement >> enclosedSetElement: anObject [
	enclosedElement := anObject
]

{ #category : #comparing }
SetElement >> hash [
	^ enclosedElement hash

]
