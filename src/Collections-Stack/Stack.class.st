Class {
	#name : #Stack,
	#superclass : #Object,
	#instVars : [
		'aCollection'
	],
	#category : #'Collections-Stack-Base'
}

{ #category : #initialize }
Stack >> initialize [ 
aCollection:= OrderedCollection new.
]

{ #category : #removing }
Stack >> pop [
	"Returns the first element and remove it from the stack."

	^aCollection  removeLast
]

{ #category : #adding }
Stack >> push: anObject [ 
	"Adds a new object of any kind on top of the stack."
	aCollection addLast: anObject.
	^ anObject.
]

{ #category : #removing }
Stack >> top [
	"Answer the first element of the stack without removing it."
	^ aCollection at: (aCollection lastIndex).
]
