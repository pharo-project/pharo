"
I am a mock exception handler used for testing purposes
"
Class {
	#name : #TKTTestExceptionHandler,
	#superclass : #TKTExceptionHandler,
	#instVars : [
		'error'
	],
	#category : #'TaskIt-Tests-Kernel'
}

{ #category : #handling }
TKTTestExceptionHandler >> error [

	^ error
]

{ #category : #handling }
TKTTestExceptionHandler >> handleException: anError [ 
	error := anError
]
