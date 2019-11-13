"
I implement a simple Stack. #push: adds a new object of any kind on top of the stack. #pop returns the first element and remove it from the stack. #top answer the first element of the stack without removing it.
"
Class {
	#name : #Stack,
	#superclass : #LinkedList,
	#category : #'Collections-Stack-Base'
}

{ #category : #removing }
Stack >> pop [
	"Returns the first element and remove it from the stack."

	^self removeFirst 
]

{ #category : #adding }
Stack >> push: anObject [ 
	"Adds a new object of any kind on top of the stack."
	
	self addFirst: anObject.
	^ anObject
]

{ #category : #accessing }
Stack >> top [
	"Answer the first element of the stack without removing it."
	
	^ self first
]
