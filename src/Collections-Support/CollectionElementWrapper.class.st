"
I am a helper class for handling the contents of some collections that need a special handling of nil values.

My main purpose is to allow a Set or WeakValueDictionary to contain nil.

Since nil represents an empty slot in those collections, it cannot be stored directly in the array of values. Instead, my instances are used as a wrapper. See implementors and senders of #asCollectionElementWrapper for wrapping, and #enclosedElement for unwrapping.
"
Class {
	#name : #CollectionElementWrapper,
	#superclass : #Object,
	#instVars : [
		'enclosedElement'
	],
	#classVars : [
		'NilElement'
	],
	#category : #'Collections-Support-Utilities'
}

{ #category : #'class initialization' }
CollectionElementWrapper class >> initialize [
	NilElement := self with: nil.

]

{ #category : #'instance creation' }
CollectionElementWrapper class >> with: anObject [
	^ self new enclosedElement: anObject
]

{ #category : #accessing }
CollectionElementWrapper class >> withNil [
	^ NilElement

]

{ #category : #comparing }
CollectionElementWrapper >> = anObject [
	super = anObject ifTrue: [ ^ true ].
	anObject class = self class ifFalse: [ ^ false ].

	^ enclosedElement = anObject enclosedElement
]

{ #category : #converting }
CollectionElementWrapper >> asCollectionElementWrapper [
	"A receiver has to be included into a collection needing a wrapper, as a distinct object.
	We need to wrap receiver in own turn, otherwise #enclosedElement will return wrong object for that collection"

	^ self class with: self
]

{ #category : #accessing }
CollectionElementWrapper >> enclosedElement [
	^ enclosedElement
]

{ #category : #accessing }
CollectionElementWrapper >> enclosedElement: anObject [
	enclosedElement := anObject
]

{ #category : #comparing }
CollectionElementWrapper >> hash [
	^ enclosedElement hash

]
