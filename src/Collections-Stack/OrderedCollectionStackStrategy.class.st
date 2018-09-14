"
I am a stack using a OrderedCollection under the hood.
"
Class {
	#name : #OrderedCollectionStackStrategy,
	#superclass : #AbstractStackStrategy,
	#instVars : [
		'orderedCollection'
	],
	#category : #'Collections-Stack-Strategies'
}

{ #category : #enumerating }
OrderedCollectionStackStrategy >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	 Starts by the element at the top of the Linked list and goes to the deepest element."

	orderedCollection do: aBlock
]

{ #category : #initialization }
OrderedCollectionStackStrategy >> initialize [
	super initialize.
	orderedCollection := OrderedCollection new
]

{ #category : #removing }
OrderedCollectionStackStrategy >> pop [
	"Remove the object at the top of the stack and returns it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ orderedCollection removeFirst
]

{ #category : #adding }
OrderedCollectionStackStrategy >> push: newObject [
	"Push newObject on the top of the stack.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	orderedCollection addFirst: newObject.
	^ newObject
]

{ #category : #accessing }
OrderedCollectionStackStrategy >> size [
	"Optimized way to get the size of the stack."
	^ orderedCollection size
]

{ #category : #accessing }
OrderedCollectionStackStrategy >> top [
	"Returns the element at the top of the stack without removing it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ orderedCollection first
]
