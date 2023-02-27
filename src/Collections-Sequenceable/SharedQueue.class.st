"
An implementation of a shared queue based on class Monitor.  Clients may may place items on the queue using nextPut: or remove them using methods like next or nextOrNil.  Items are removed in first-in first-out (FIFO) order.  It is safe for multiple threads to access the same shared queue, which is why this is a ""shared"" queue.

[monitor] is used to synchronize access from multiple threads.

[items] is an ordered collection holding the items that are in the queue.  New items are added  at the end, and old items are removed from the beginning.

All methods must hold the monitor while they run.

"
Class {
	#name : #SharedQueue,
	#superclass : #Stream,
	#instVars : [
		'monitor',
		'items'
	],
	#category : #'Collections-Sequenceable-Base'
}

{ #category : #'instance creation' }
SharedQueue class >> new [
	^self basicNew initialize
]

{ #category : #testing }
SharedQueue >> atEnd [
	^ self isEmpty
]

{ #category : #accessing }
SharedQueue >> contents [
	" To be compliant with stream API but is really needed? "
	^ monitor critical:
		[ items copy ]
]

{ #category : #accessing }
SharedQueue >> findFirst: aBlock [
	"Answer the next object that satisfies aBlock, skipping any intermediate objects.
	If no such object has been queued, answer <nil> and leave me intact."


	^monitor critical: [ | index |
		index := items findFirst: aBlock.
		index = 0 ifTrue: [
			nil ]
		ifFalse: [
			items removeAt: index ] ]
]

{ #category : #flushing }
SharedQueue >> flush [
	"Remove from the queue all objects"
	monitor critical: [
		items := items removeAll]
]

{ #category : #flushing }
SharedQueue >> flushAllSuchThat: aBlock [
	"Remove from the queue all objects that satisfy aBlock."
	monitor critical: [
		items := items reject: aBlock]
]

{ #category : #initialization }
SharedQueue >> initialize [
	super initialize.
	monitor := Monitor new.
	items := OrderedCollection new
]

{ #category : #size }
SharedQueue >> isEmpty [
	^monitor critical: [ items isEmpty ]
]

{ #category : #accessing }
SharedQueue >> next [
	^monitor critical: [
		monitor waitWhile: [ items isEmpty ].
		items removeFirst ]
]

{ #category : #accessing }
SharedQueue >> nextOrNil [
	^monitor critical: [
		items isEmpty ifTrue: [ nil ] ifFalse: [ items removeFirst ] ]
]

{ #category : #accessing }
SharedQueue >> nextOrNilSuchThat: aBlock [
	"Answer the next object that satisfies aBlock, skipping any intermediate objects.
	If no object has been sent, answer <nil> and leave me intact.
	NOTA BENE:  aBlock MUST NOT contain a non-local return (^)."

	^monitor critical: [
		| index |
		index := items findFirst: aBlock.
		index = 0
			ifTrue: [nil]
			ifFalse: [items removeAt: index]
	]
]

{ #category : #accessing }
SharedQueue >> nextPut: anObject [
	monitor critical: [
		items addLast: anObject.
		monitor signal].
	^ anObject
]

{ #category : #accessing }
SharedQueue >> peek [
	"Answer the object that was sent through the receiver first and has not
	yet been received by anyone but do not remove it from the receiver. If
	no object has been sent, return nil"
	^monitor critical: [
		items isEmpty ifTrue: [ nil ] ifFalse: [ items first ] ]
]

{ #category : #copying }
SharedQueue >> postCopy [
	super postCopy.
	monitor critical:
		[items := items copy.
		monitor := Monitor new]
]

{ #category : #printing }
SharedQueue >> printOn: aStream [
	aStream
		nextPutAll: self class name;
		nextPutAll: ' with ';
		print: items size;
	 	nextPutAll: ' items'
]

{ #category : #accessing }
SharedQueue >> removeAll [
       monitor critical: [
		items removeAll ]
]

{ #category : #accessing }
SharedQueue >> size [
	^monitor critical: [ items size ]
]
