"
I'm a special working session for test purposes.
I provide a way to escape from the UIManager. 
"
Class {
	#name : #TestWorkingSession,
	#superclass : #WorkingSession,
	#instVars : [
		'errors',
		'errorHandler'
	],
	#category : #System-SessionManager-Tests
}

{ #category : #accessing }
TestWorkingSession >> errorHandler [
	^ errorHandler ifNil: [ self ]
]

{ #category : #accessing }
TestWorkingSession >> errorHandler: anErrorHandler [
	errorHandler := anErrorHandler
]

{ #category : #accessing }
TestWorkingSession >> errors [
	^ errors
]

{ #category : #'deferred startup actions' }
TestWorkingSession >> executeDeferredStartupActions: isImageStarting [
	"do nothing"
]

{ #category : #'error handling' }
TestWorkingSession >> handleError: anError [
	errors add: anError
]

{ #category : #initialization }
TestWorkingSession >> initialize [
	super initialize.
	errors := OrderedCollection new
]

{ #category : #'startup - shutdown' }
TestWorkingSession >> start: isImageStarting [
	"do not modify vm parameters, external objects"
	self runStartup: isImageStarting.
]
