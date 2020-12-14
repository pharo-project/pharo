Class {
	#name : #TFTestCallbackExceptionHandler,
	#superclass : #Object,
	#instVars : [
		'lastException',
		'available'
	],
	#category : #'ThreadedFFI-Tests'
}

{ #category : #handling }
TFTestCallbackExceptionHandler >> handleExceptionDuring: aBlockClosure [ 
	
	aBlockClosure on: Exception do: [ :e |
		lastException := e freeze.
		available signal ]
]

{ #category : #handling }
TFTestCallbackExceptionHandler >> initialize [

	super initialize.
	available := Semaphore new.
]

{ #category : #accessing }
TFTestCallbackExceptionHandler >> lastException [

	^ lastException
]

{ #category : #handling }
TFTestCallbackExceptionHandler >> wait [

	available wait
]
