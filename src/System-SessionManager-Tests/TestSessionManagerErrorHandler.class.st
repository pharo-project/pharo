"
I'm an startup error manager used for test purposes.
I provide a way to not use the default session manager.

"
Class {
	#name : 'TestSessionManagerErrorHandler',
	#superclass : 'SessionManagerErrorHandler',
	#instVars : [
		'sessionManager'
	],
	#category : 'System-SessionManager-Tests',
	#package : 'System-SessionManager-Tests'
}

{ #category : 'accessing' }
TestSessionManagerErrorHandler >> sessionManager [

	^ sessionManager
]

{ #category : 'accessing' }
TestSessionManagerErrorHandler >> sessionManager: anObject [

	sessionManager := anObject
]
