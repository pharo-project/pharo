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

{ #category : #accessing }
AtomicQueueItem >> isCircular [
	^ next == self
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
