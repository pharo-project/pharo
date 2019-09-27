"
This class is deprecated because while exposing the API of a Stack, it was also inheriting form LinkedList which allowed people to use LinkedList API.

This was wrong because if one needs something else than #pop, #push, #top and #size, then it means that what one is looking for is a LinkedList.
Thus, one should explicitely use it.
"
Class {
	#name : #Stack,
	#superclass : #LinkedList,
	#category : #Deprecated80
}

{ #category : #deprecation }
Stack class >> isDeprecated [
	^ true
]

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
