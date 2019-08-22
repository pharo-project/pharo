"
I am a lookup key (acting like an association but) holding only weakly on my value.

I also store the kind of association that was given to the WeakValueDictionary to be able to recreate it in case it is necessary.
"
Class {
	#name : #WeakValueAssociation,
	#superclass : #LookupKey,
	#type : #weak,
	#instVars : [
		'originalAssociationClass'
	],
	#category : #'Collections-Support-Associations'
}

{ #category : #'instance creation' }
WeakValueAssociation class >> key: anObject value: bObject [
	^ self new key: anObject value: bObject
]

{ #category : #'instance creation' }
WeakValueAssociation class >> new [
	^ self new: 1
]

{ #category : #converting }
WeakValueAssociation >> asOriginalAssociation [
	^ self originalAssociationClass ifNotNil: [ :class | class key: self key value: self value enclosedElement ] ifNil: [ self ]
]

{ #category : #accessing }
WeakValueAssociation >> key: aKey value: anObject [ 
	"Store the arguments as the variables of the receiver."

	key := aKey.
	self value: anObject
]

{ #category : #accessing }
WeakValueAssociation >> originalAssociationClass [
	^ originalAssociationClass
]

{ #category : #accessing }
WeakValueAssociation >> originalAssociationClass: anObject [
	originalAssociationClass := anObject
]

{ #category : #evaluating }
WeakValueAssociation >> value [
	^ self at: 1
]

{ #category : #accessing }
WeakValueAssociation >> value: anObject [
	"Store the argument, anObject, as the value of the receiver."

	self at: 1 put: anObject
]
