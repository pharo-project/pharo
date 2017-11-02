"
OutOfMemory is signaled when an allocation fails due to not having enough memory. Its default action signals the low-space semaphore.
Originally suggested by A. Raab.
"
Class {
	#name : #OutOfMemory,
	#superclass : #Error,
	#category : 'Kernel-Exceptions'
}

{ #category : #handling }
OutOfMemory >> defaultAction [
	Smalltalk signalLowSpace.
]

{ #category : #testing }
OutOfMemory >> isResumable [

	^true
]
