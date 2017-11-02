"
I am a render operation, used to send commands (which can be asynchronous or synchronous) to the render thread. 
"
Class {
	#name : #OSWindowRenderThreadOperation,
	#superclass : #Object,
	#instVars : [
		'operation',
		'semaphore',
		'blocking',
		'failed'
	],
	#category : 'OSWindow-Core-Extras'
}

{ #category : #'instance creation' }
OSWindowRenderThreadOperation class >> new: anOperation [
	^ self new: anOperation blocking: false
]

{ #category : #'instance creation' }
OSWindowRenderThreadOperation class >> new: anOperation blocking: isBlocking [
	^ self basicNew initWithOperation: anOperation blocking: isBlocking; yourself
]

{ #category : #waiting }
OSWindowRenderThreadOperation >> blocking [
	^ blocking
]

{ #category : #execute }
OSWindowRenderThreadOperation >> execute [
	[ 
		[
			failed := false.
			operation value
		] on: Exception do: [ :ex |
			failed := true.
			ex freeze.
			[ ex debug ] fork.
		]
	] ensure: [ semaphore signal ]
]

{ #category : #accessing }
OSWindowRenderThreadOperation >> failed [
	^ failed
]

{ #category : #initialize }
OSWindowRenderThreadOperation >> initWithOperation: anOperation [
	semaphore := Semaphore new.
	operation := anOperation.
	
	
]

{ #category : #initialize }
OSWindowRenderThreadOperation >> initWithOperation: anOperation blocking: isBlocking [
	semaphore := Semaphore new.
	operation := anOperation.
	blocking := isBlocking
	
	
]

{ #category : #waiting }
OSWindowRenderThreadOperation >> wait [
	semaphore wait
]
