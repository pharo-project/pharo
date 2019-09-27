"
I am a parametrizable stack. That is to say, I provide the interface of a stack but depending on my #strategy, I can use a LinkedList, an OrderedCollection, etc for implementation details.
"
Class {
	#name : #NewStack,
	#superclass : #Collection,
	#instVars : [
		'strategy'
	],
	#category : #'Collections-Stack-Base'
}

{ #category : #factory }
NewStack class >> usingLinkedList [
 	"Returns an instance of Stack using a LinkedList under the hood."
 	^ self usingStrategy: LinkedListStackStrategy new
]

{ #category : #factory }
NewStack class >> usingOrderedCollection [
 	"Returns an instance of Stack using a OrderedCollection under the hood."
 	^ self usingStrategy: OrderedCollectionStackStrategy new
]

{ #category : #'instance creation' }
NewStack class >> usingStrategy: aStackStrategy [
 	"Given a aStackStrategy, returns an instance of Stack using this strategy under the hood."
 	^ self new
 		initializeWith: aStackStrategy;
 		yourself
]

{ #category : #adding }
NewStack >> add: newObject [
 	"Include newObject as one of the receiver's elements. Answer newObject. 
 	ArrayedCollections cannot respond to this message."

  	^ self push: newObject
]

{ #category : #enumerating }
NewStack >> do: aBlock [
 	"Evaluate aBlock with each of the receiver's elements as the argument."

  	self strategy do: aBlock
]

{ #category : #initialization }
NewStack >> initialize [
	super initialize.
	strategy := LinkedListStackStrategy new
]

{ #category : #initialization }
NewStack >> initializeWith: aStrategy [
	strategy := aStrategy
]

{ #category : #removing }
NewStack >> pop [
	" Remove the object at the top of the stack and return it.

  	 (Stack new push: 1; push: 2; pop) >>> 2
	"
 	^ self strategy pop
]

{ #category : #adding }
NewStack >> push: newObject [
 	"Push newObject on the top of the stack."
 	self strategy push: newObject.
 	^ newObject
]

{ #category : #removing }
NewStack >> remove: oldObject ifAbsent: anExceptionBlock [
	"A stack should not implement that.
	 Use a linked list if you really need this feature.
	"
	self shouldNotImplement
]

{ #category : #removing }
NewStack >> removeAll [
	self strategy reset
]

{ #category : #accessing }
NewStack >> strategy [
	^ strategy
]

{ #category : #accessing }
NewStack >> top [
 	"Returns the object at the top of the stack without removing it."
 	^ self strategy top
]
