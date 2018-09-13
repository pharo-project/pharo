"
I am an abstract Stack.

Addtionally to Collection messages, I understand the following famous messages: 
- #push: to push an object at the top of the stack.
- #top to get the object at the top of the stack without removing it.
- #pop to remove the object at the top of the stack and return it.
"
Class {
	#name : #AbstractStack,
	#superclass : #Collection,
	#category : #'Collections-Stack-Base'
}

{ #category : #adding }
AbstractStack >> add: newObject [ 
	self push: newObject.
	^ newObject
]

{ #category : #enumerating }
AbstractStack >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	 Starts by the element at the top of the Linked list and goes to the deepest element."

	self subclassResponsibility
]

{ #category : #removing }
AbstractStack >> pop [
	"Remove the object at the top of the stack and returns it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ self subclassResponsibility
]

{ #category : #adding }
AbstractStack >> push: newObject [
	"Push newObject on the top of the stack.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	self subclassResponsibility
]

{ #category : #removing }
AbstractStack >> remove: oldObject ifAbsent: anExceptionBlock [ 
	"Remove oldObject from the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject. ArrayedCollections cannot 
	respond to this message."

	^ self shouldNotImplement
]

{ #category : #accessing }
AbstractStack >> top [
	"Returns the element at the top of the stack without removing it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ self subclassResponsibility
]
