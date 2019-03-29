"
I implement a simple Stack. #push: adds a new object of any kind on top of the stack. #pop returns the first element and remove it from the stack. #top answer the first element of the stack without removing it.
"
Class {
	#name : #Stack,
	#superclass : #Collection,
	#instVars : [
		'aCollection'
	],
	#category : #'Collections-Stack-Base'
}

{ #category : #enumerating }
Stack >> do: aBlockClosure [ 
	aCollection do: aBlockClosure.
	

]

{ #category : #'as yet unclassified' }
Stack >> ifEmpty: emptyBlock ifNotEmpty: notEmptyBlock [ 
	self isEmpty ifTrue: [ ^emptyBlock value ].
	^notEmptyBlock cull: self
]

{ #category : #'as yet unclassified' }
Stack >> ifNotEmpty: aBlockClosure [ 
	    ^self isEmpty
          ifTrue: [self]
          ifFalse: [aBlockClosure cull: self] 
]

{ #category : #initialize }
Stack >> initialize [ 
aCollection:= OrderedCollection new.
]

{ #category : #testing }
Stack >> isEmpty [
	^ aCollection isEmpty.
]

{ #category : #'as yet unclassified' }
Stack >> isEmptyOrNil [
	^ aCollection isEmptyOrNil.
]

{ #category : #testing }
Stack >> isNotEmpty [
	^ aCollection isNotEmpty.
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
Stack >> size [ 
	^aCollection size
]

{ #category : #removing }
Stack >> top [
	"Answer the first element of the stack without removing it."
	^ aCollection at: (aCollection lastIndex).
]
