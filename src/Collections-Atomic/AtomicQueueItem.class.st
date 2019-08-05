"
I am a queue item, used by atomic queues.
All my new instances are circular i.e. next = self
"
Class {
	#name : #AtomicQueueItem,
	#superclass : #Object,
	#instVars : [
		'next',
		'object'
	],
	#category : #'Collections-Atomic-Elements'
}

{ #category : #initialization }
AtomicQueueItem >> initialize [
	"make circular"
	super initialize.
	next := self.
]

{ #category : #testing }
AtomicQueueItem >> isCircular [
	^ next == self
]

{ #category : #testing }
AtomicQueueItem >> isZombie [
	"Answer a boolean indicating that the receiver doesn't have a real object.
	This should really check that the object is the queue's dummy, but since the item doesn't know which queue it is part of, use the object class instead."

	^object class = self class
]

{ #category : #accessing }
AtomicQueueItem >> makeCircular [
	"Make a receiver circular, i.e. point to itself,
	answer the old value of next variable. 
	Note, this operation should be atomic"
	
	| temp |

	" atomic swap here"
	temp := next.
	next := self.

	^ temp
]

{ #category : #accessing }
AtomicQueueItem >> next [
	"Answer the value of next"

	^ next
]

{ #category : #accessing }
AtomicQueueItem >> next: anObject [
	"Set the value of next"

	next := anObject
]

{ #category : #accessing }
AtomicQueueItem >> object [
	"Answer the value of object"

	^ object
]

{ #category : #accessing }
AtomicQueueItem >> object: anObject [
	"Set the value of object"

	object := anObject
]

{ #category : #printing }
AtomicQueueItem >> printOn: aStream [

	super printOn: aStream.
	aStream << '('.
	self isZombie ifTrue: 
		[ aStream << '<zombie>' ]
	ifFalse:
		[ object printOn: aStream ].
	self isCircular ifTrue: 
		[ aStream << ', Circular' ].
	aStream
		<< ')'
]
