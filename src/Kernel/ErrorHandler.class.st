Class {
	#name : 'ErrorHandler',
	#superclass : 'Object',
	#classVars : [
		'DefaultErrorHandler'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'accessing' }
ErrorHandler class >> defaultErrorHandler [

	^ DefaultErrorHandler ifNil: [ self ]
]

{ #category : 'accessing' }
ErrorHandler class >> defaultErrorHandler: anErrorHandler [

	DefaultErrorHandler := anErrorHandler
]

{ #category : 'error handling' }
ErrorHandler class >> handleError: anError [

	Smalltalk exit: 1
]

{ #category : 'error handling' }
ErrorHandler class >> handleNotification: aNotification [

	aNotification resume
]

{ #category : 'error handling' }
ErrorHandler class >> handleWarning: aWarning [

	aWarning resume
]
