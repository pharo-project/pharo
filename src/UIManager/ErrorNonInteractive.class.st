"
This error thrown when image runs in non-interactive mode (no user intervention possible nor expected),
but application trying to use tools which require user's intervention.
For example, in headless image application trying to query a user name, or dialog to confirm with proceeding some action.
"
Class {
	#name : #ErrorNonInteractive,
	#superclass : #Error,
	#instVars : [
		'exception'
	],
	#category : #'UIManager-Errors'
}

{ #category : #signalling }
ErrorNonInteractive class >> signalForException: anError [

	^ self new
		exception: anError;
		signal
]

{ #category : #exceptiondescription }
ErrorNonInteractive >> defaultAction [
	" log error and quit "
	
	^ UIManager default 
		quitFrom: self signalerContext 
		withMessage: self description
]

{ #category : #accessing }
ErrorNonInteractive >> description [
	^ String streamContents: [ :s |
		s nextPutAll: 'An attempt to use interactive tools detected, while in non-interactive mode'. 
		self messageText  ifNotEmpty: [ :message| 
			s cr; tab; nextPutAll: message ].
		self exception ifNotNil: [ :ex |
			s cr; tab; print: ex ]].
]

{ #category : #accessing }
ErrorNonInteractive >> exception [

	^ exception
]

{ #category : #accessing }
ErrorNonInteractive >> exception: anError [

	exception := anError
]
