"
I am a parametrizable stack. That is to say, I provide the interface of a stack but depending on my #strategy, I can use a LinkedList, an OrderedCollection, etc for implementation details.
"
Class {
	#name : #Stack,
	#superclass : #Collection,
	#instVars : [
		'strategy'
	],
	#category : #'Collections-Stack-Base'
}

{ #category : #'instance creation' }
Stack class >> usingLinkedList [
	"Returns an instance of Stack using a LinkedList under the hood."
	^ self usingStrategy: LinkedListStackStrategy new
]

{ #category : #'instance creation' }
Stack class >> usingOrderedCollection [
	"Returns an instance of Stack using a OrderedCollection under the hood."
	^ self usingStrategy: OrderedCollectionStackStrategy new
]

{ #category : #'instance creation' }
Stack class >> usingStrategy: aStackStrategy [
	"Given a aStackStrategy, returns an instance of Stack using this strategy under the hood."
	^ self new
		strategy: aStackStrategy;
		yourself
]

{ #category : #adding }
Stack >> add: newObject [ 
	"Include newObject as one of the receiver's elements. Answer newObject. 
	ArrayedCollections cannot respond to this message."

	self strategy push: newObject.
	^ newObject
]

{ #category : #enumerating }
Stack >> do: aBlock [ 
	"Evaluate aBlock with each of the receiver's elements as the argument."

	self strategy do: aBlock
]

{ #category : #initialization }
Stack >> initialize [
	super initialize.
	self
		strategy: LinkedListStackStrategy new
]

{ #category : #removing }
Stack >> pop [
	" Remove the object at the top of the stack and return it.
	 (Stack new push: 1; push: 2; pop) >>> 2
	"
	^ self strategy pop
]

{ #category : #adding }
Stack >> push: newObject [
	" Push newObject on the top of the stack.
	"
	self strategy push: newObject.
	^ newObject
]

{ #category : #removing }
Stack >> remove: oldObject ifAbsent: anExceptionBlock [ 
	"Remove oldObject from the receiver's elements. If several of the 
	elements are equal to oldObject, only one is removed. If no element is 
	equal to oldObject, answer the result of evaluating anExceptionBlock. 
	Otherwise, answer the argument, oldObject. ArrayedCollections cannot 
	respond to this message."

	self shouldNotImplement
]

{ #category : #accessing }
Stack >> size [
	^ self strategy size
]

{ #category : #private }
Stack >> strategy [
	^ strategy
]

{ #category : #private }
Stack >> strategy: anObject [
	strategy := anObject
]

{ #category : #accessing }
Stack >> top [
	"Returns the object at the top of the stack without removing it."
	^ self strategy top
]
