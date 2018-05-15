"
This is a thread-safe LIFO (last-in-first-out) queue (also known as stack) implementation,
based on atomic operations.

"
Class {
	#name : #LIFOQueue,
	#superclass : #AtomicCollection,
	#instVars : [
		'head'
	],
	#category : #'Collections-Atomic-Base'
}

{ #category : #'stack-compliant protocol' }
LIFOQueue >> errorEmptyStack [
	self error: 'this stack is empty'
]

{ #category : #accessing }
LIFOQueue >> fastPeek [
	"Answer a top-most value without removing it from queue.
	Answer nil, if queue is empty or currently blocked by other process,
	which reading from queue"
	
	| item result |
	item := head.
	
	[ (result := item object) == item ] whileTrue: [ 
		item isCircular ifTrue: [ ^ nil ].
		(item := item next) ifNil: [ ^ nil ] ].
	
	^ result
]

{ #category : #initialization }
LIFOQueue >> initialize [
	| dummy |
	dummy := self newItem.
	dummy next: nil; object: dummy.
	head := dummy.
]

{ #category : #accessing }
LIFOQueue >> next [
	| dummy tail |

	dummy := self newItem.
	dummy object: dummy.

	"this is atomic"
	tail := head.
	head := dummy.

	"skip over dummies"
	[ tail object == tail ] whileTrue: [ 
		[ tail isCircular ] whileTrue: [ self yield ].
		(tail := tail next) ifNil: [  | result |
			"queue is empty. block until new items appear"
			head == dummy ifTrue: [ self signalNoMoreItems ].
			[ head == dummy ] whileTrue: [ self waitForNewItems ].
			dummy next: nil. 
			result := self next.
			^ result ] ].

	dummy next: tail next.

	^ tail object
	
]

{ #category : #accessing }
LIFOQueue >> nextIfNone: aBlock [
	| dummy tail |

	dummy := self newItem.
	dummy object: dummy.

	tail := head.
	head := dummy.

	"skip over dummies"
	[ tail object == tail ] whileTrue: [ 
		[ tail isCircular ] whileTrue: [ self yield ].
		(tail := tail next) ifNil: [ 
			dummy next: nil. 
			dummy == head ifTrue: [ self signalNoMoreItems].
			^ aBlock value ] ].

	dummy next: tail next.

	^ tail object
	
]

{ #category : #accessing }
LIFOQueue >> nextOrNil [
	^ self nextIfNone: [ nil ]
]

{ #category : #accessing }
LIFOQueue >> nextPut: anObject [
	
	| newItem oldHead |

	newItem := self newItem.
	newItem object: anObject.

	"this is atomic"
	oldHead := head.
	head := newItem.
	
	newItem next: oldHead.
	
	self signalAddedNewItem.
	^ anObject
]

{ #category : #accessing }
LIFOQueue >> peek [
	"answer a top-most value without removing it,  or nil, if queue is empty.
	May block if there's another process reading from queue"
	
	| item result |
	item := head.
	
	[ (result := item object) == item ] whileTrue: [ 
		[ item isCircular ] whileTrue: [ self yield ].
		(item := item next) ifNil: [ ^ nil ] ].
	
	^ result
]

{ #category : #'stack-compliant protocol' }
LIFOQueue >> pop [

	^ self nextIfNone: [ self errorEmptyStack ]
]

{ #category : #'stack-compliant protocol' }
LIFOQueue >> push: anObject [
	^ self nextPut: anObject
]
