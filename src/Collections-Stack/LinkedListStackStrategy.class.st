"
I am a stack using a LinkedList under the hood.
"
Class {
	#name : #LinkedListStackStrategy,
	#superclass : #AbstractStackStrategy,
	#instVars : [
		'linkedList'
	],
	#category : #'Collections-Stack-Strategies'
}

{ #category : #enumerating }
LinkedListStackStrategy >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	 Starts by the element at the top of the Linked list and goes to the deepest element."

	linkedList do: aBlock
]

{ #category : #initialization }
LinkedListStackStrategy >> initialize [
	super initialize.
	linkedList := LinkedList new
]

{ #category : #removing }
LinkedListStackStrategy >> pop [
	"Remove the object at the top of the stack and returns it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ linkedList removeFirst
]

{ #category : #adding }
LinkedListStackStrategy >> push: newObject [
	"Push newObject on the top of the stack.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	linkedList addFirst: newObject.
	^ newObject
]

{ #category : #accessing }
LinkedListStackStrategy >> size [
	"Optimized way to get the size of the stack."
	^ linkedList size
]

{ #category : #accessing }
LinkedListStackStrategy >> top [
	"Returns the element at the top of the stack without removing it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ linkedList first
]
