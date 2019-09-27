"
I am a stack using a OrderedCollection under the hood.

I store the top of the stack at the bottom of the collection as it is more efficient.
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
	orderedCollection reverseDo: aBlock
]

{ #category : #initialization }
OrderedCollectionStackStrategy >> initialize [
	super initialize.
	self reset
]

{ #category : #removing }
OrderedCollectionStackStrategy >> pop [
 	^ orderedCollection removeLast
]

{ #category : #adding }
OrderedCollectionStackStrategy >> push: newObject [
 	orderedCollection addLast: newObject.
 	^ newObject
]

{ #category : #initialization }
OrderedCollectionStackStrategy >> reset [
	orderedCollection := OrderedCollection new
]

{ #category : #accessing }
OrderedCollectionStackStrategy >> size [
 	^ orderedCollection size
]

{ #category : #accessing }
OrderedCollectionStackStrategy >> top [
 	^ orderedCollection last
]
