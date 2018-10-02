"
I am an abstract Stack.

Addtionally to Collection messages, I understand the following famous messages: 
- #push: to push an object at the top of the stack.
- #top to get the object at the top of the stack without removing it.
- #pop to remove the object at the top of the stack and return it.
"
Class {
	#name : #AbstractStackStrategy,
	#superclass : #Object,
	#category : #'Collections-Stack-Strategies'
}

{ #category : #enumerating }
AbstractStackStrategy >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	 Starts by the element at the top of the Linked list and goes to the deepest element."

	self subclassResponsibility
]

{ #category : #removing }
AbstractStackStrategy >> pop [
	"Remove the object at the top of the stack and returns it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ self subclassResponsibility
]

{ #category : #adding }
AbstractStackStrategy >> push: newObject [
	"Push newObject on the top of the stack.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	self subclassResponsibility
]

{ #category : #accessing }
AbstractStackStrategy >> size [
	^ self strategy size
]

{ #category : #accessing }
AbstractStackStrategy >> top [
	"Returns the element at the top of the stack without removing it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ self subclassResponsibility
]
