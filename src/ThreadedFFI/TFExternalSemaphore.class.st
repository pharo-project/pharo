"
I am container for a semaphore which will be automatically registered as external on initialization.
This is to be used as part of the external semaphores pool.
Inside me you can find: 

- semaphore 
- index 

index is the external semaphore index to be used on primitive calls.

"
Class {
	#name : #TFExternalSemaphore,
	#superclass : #Object,
	#instVars : [
		'semaphore',
		'index'
	],
	#category : #'ThreadedFFI-Base'
}

{ #category : #accessing }
TFExternalSemaphore >> index [

	^ index
]

{ #category : #initialization }
TFExternalSemaphore >> initialize [
	
	super initialize.
	semaphore := Semaphore new.
	index := Smalltalk registerExternalObject: semaphore
]

{ #category : #initialization }
TFExternalSemaphore >> release [

	super release.
	Smalltalk unregisterExternalObject: semaphore.

]

{ #category : #initialization }
TFExternalSemaphore >> reset [

	semaphore consumeAllSignals
]

{ #category : #accessing }
TFExternalSemaphore >> semaphore [

	^ semaphore
]
