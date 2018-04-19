"
I am a lookup key (acting like an association but) holding only weakly on my value.
"
Class {
	#name : #WeakValueAssociation,
	#superclass : #LookupKey,
	#type : #weak,
	#category : #'Collections-Support'
}

{ #category : #'instance creation' }
WeakValueAssociation class >> key: anObject value: bObject [
	^ self new key: anObject value: bObject
]

{ #category : #'instance creation' }
WeakValueAssociation class >> new [
	^ self new: 1
]

{ #category : #accessing }
WeakValueAssociation >> key: aKey value: anObject [ 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	self value: anObject
]

{ #category : #evaluating }
WeakValueAssociation >> value [
	^self at: 1
]

{ #category : #accessing }
WeakValueAssociation >> value: anObject [ 
	"Store the argument, anObject, as the value of the receiver."

	self at: 1 put: anObject
]
