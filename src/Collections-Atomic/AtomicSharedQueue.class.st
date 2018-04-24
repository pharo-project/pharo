"
I am using semaphore to signal/wait for new items available in queue

I am introducing extra protocol - #next, which blocks the sender until 
it can successfully fetch next item from queue.

"
Class {
	#name : #AtomicSharedQueue,
	#superclass : #WaitfreeQueue,
	#instVars : [
		'availSema'
	],
	#category : #'Collections-Atomic-Base'
}

{ #category : #initialization }
AtomicSharedQueue >> initialize [
	super initialize.
	availSema := Semaphore new.
	
]

{ #category : #accessing }
AtomicSharedQueue >> next [
	"Fetch the next item from queue. If queue is locked or empty, block the sender until operation can complete"
	 
	| result |
	
	[ | keepWaiting | 
		keepWaiting := false. 
		result := self nextIfNone: [ keepWaiting := true ]. 
		keepWaiting ] whileTrue: [ self waitForNewItems ].

	^ result
]

{ #category : #signaling }
AtomicSharedQueue >> signalAddedNewItem [
	availSema signal
]

{ #category : #signaling }
AtomicSharedQueue >> signalNoMoreItems [
	"queue is empty, reset sema signals"
	availSema consumeAllSignals 
]

{ #category : #signaling }
AtomicSharedQueue >> waitForNewItems [
	availSema wait
]
