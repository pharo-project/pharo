"
A trait for test purposes
"
Class {
	#name : #WeakOrderedCollectionTest,
	#superclass : #TestCase,
	#category : 'Collections-Tests-Weak'
}

{ #category : #tests }
WeakOrderedCollectionTest >> testWeakOrderedCollectionAllGarbageCollected [
	| anArray anotherObject weakOrderedCollection |
	
	anArray := OrderedCollection new.
	anotherObject := Time new. 
	anotherObject seconds: 40.
	anArray add: Date today.
	anArray add: anotherObject.

	weakOrderedCollection := WeakOrderedCollection new.
	weakOrderedCollection 
		add: anArray;
		add: anotherObject.
		
	"Let's add lot of stuff to be sure #grow is send."
	(1 to: 1000) do: [:index | weakOrderedCollection add: Date today ].
	
	anotherObject := nil.
	anArray := nil.
	
	3 timesRepeat: [Smalltalk garbageCollect].
	self assert: weakOrderedCollection asSet size equals: 1.
	self assert: (weakOrderedCollection asSet includes: nil)


]

{ #category : #tests }
WeakOrderedCollectionTest >> testWeakOrderedCollectionSomeGarbageCollected [
	| anArray anotherObject weakOrderedCollection |
	
	anArray := OrderedCollection new.
	anotherObject := Time new. 
	anotherObject seconds: 40.
	anArray add: Duration new.
	anArray add: anotherObject.

	weakOrderedCollection := WeakOrderedCollection new.
	weakOrderedCollection 
		add: anArray;
		add: anotherObject.
		
	"Let's add lot of stuff to be sure #grow is send."
	(1 to: 1000) do: [:index | weakOrderedCollection add: Date today ].
	
	anArray := nil.
	
	3 timesRepeat: [Smalltalk garbageCollect].
	self assert: weakOrderedCollection asSet size equals: 2.
	self assert: (weakOrderedCollection asSet includes: anotherObject).
	self assert: ((weakOrderedCollection asSet select: [:each | each class = Duration]) isEmpty).
	self assert: ((weakOrderedCollection asSet select: [:each | each class = OrderedCollection]) isEmpty).


]
