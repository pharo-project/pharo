"
I am an abstract class delegating error management of unhandled errors to one of my subclass depending on the image state (starting, non interactive, interactive..)
"
Class {
	#name : 'ErrorHandler',
	#superclass : 'Object',
	#classVars : [
		'Default'
	],
	#category : 'Kernel-Exceptions',
	#package : 'Kernel',
	#tag : 'Exceptions'
}

{ #category : 'accessing' }
ErrorHandler class >> default [

	^ Default ifNil: [
			  self default: self forCurrentSystemConfiguration new.
			  Default ]
]

{ #category : 'accessing' }
ErrorHandler class >> default: anErrorManager [

	Default := anErrorManager
]

{ #category : 'initialization' }
ErrorHandler class >> forCurrentSystemConfiguration [

	^ (self allSubclasses
		   detect: [ :any | any isValidForCurrentSystemConfiguration and: [ any subclasses noneSatisfy: #isValidForCurrentSystemConfiguration ] ]
		   ifNone: [ NonInteractiveErrorHandler ]) new
]

{ #category : 'testing' }
ErrorHandler class >> isValidForCurrentSystemConfiguration [

	^ self subclassResponsibility
]

{ #category : 'error handling' }
ErrorHandler >> handleError: anError [

	self handleError: anError log: true
]

{ #category : 'error handling' }
ErrorHandler >> handleError: anError log: shouldLog [
	"Provide a default handling for unhandled error."

	self subclassResponsibility
]

{ #category : 'error handling' }
ErrorHandler >> handleNotification: aNotification [

	"by default, do nothing"

	aNotification resume
]

{ #category : 'error handling' }
ErrorHandler >> handleWarning: aWarning [
	"Handle warning notification. Should be overidden by subclass."

	self subclassResponsibility
]

{ #category : 'error handling' }
ErrorHandler >> logError: anError [

	Smalltalk logError: anError messageText inContext: anError signalerContext
]
