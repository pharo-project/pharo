"
I am an abstract exception handler. When an unhandled error happens in a runner, it is forwarded to an exception handler using the #handleException: message.

Subclasses of me are meant to define different exception handling behaviours such as debugging or logging.
"
Class {
	#name : #TKTExceptionHandler,
	#superclass : #Object,
	#category : #'TaskIt-Kernel'
}

{ #category : #handling }
TKTExceptionHandler class >> handleException: anError [
	^ self new handleException: anError
]

{ #category : #handling }
TKTExceptionHandler class >> isDebuggingCompatible [
	^ false
]

{ #category : #handling }
TKTExceptionHandler >> handleException: anError [ 
	
	anError debug 
]
