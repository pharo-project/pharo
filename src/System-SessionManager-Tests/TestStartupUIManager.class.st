"
I'm an startup UI manager used for test purposes.
I provide a way to not use the default session manager.

"
Class {
	#name : #TestStartupUIManager,
	#superclass : #StartupUIManager,
	#instVars : [
		'sessionManager'
	],
	#category : 'System-SessionManager-Tests'
}

{ #category : #accessing }
TestStartupUIManager >> sessionManager [
	^ sessionManager
]

{ #category : #accessing }
TestStartupUIManager >> sessionManager: aSessionManager [
	sessionManager := aSessionManager
]
