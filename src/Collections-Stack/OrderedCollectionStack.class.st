"
I am a stack using a OrderedCollection under the hood.
"
Class {
	#name : #OrderedCollectionStack,
	#superclass : #AbstractStack,
	#instVars : [
		'orderedCollection'
	],
	#category : #'Collections-Stack-Base'
}

{ #category : #enumerating }
OrderedCollectionStack >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument.
	 Starts by the element at the top of the Linked list and goes to the deepest element."

	orderedCollection do: aBlock
]

{ #category : #initialization }
OrderedCollectionStack >> initialize [
	super initialize.
	orderedCollection := OrderedCollection new
]

{ #category : #removing }
OrderedCollectionStack >> pop [
	"Remove the object at the top of the stack and returns it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ orderedCollection removeFirst
]

{ #category : #adding }
OrderedCollectionStack >> push: newObject [
	"Push newObject on the top of the stack.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	orderedCollection addFirst: newObject.
	^ newObject
]

{ #category : #accessing }
OrderedCollectionStack >> size [
	"Optimized way to get the size of the stack."
	^ orderedCollection size
]

{ #category : #accessing }
OrderedCollectionStack >> top [
	"Returns the element at the top of the stack without removing it.
	 The implementation depends on the internal representation of the stack.
	 Any subclass should implement this method properly."
	^ orderedCollection first
]
