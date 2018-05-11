"
SUnit tests for unregistration from the Pharo session manager
"
Class {
	#name : #SessionManagerUnregistrationTest,
	#superclass : #TestCase,
	#instVars : [
		'sessionManager'
	],
	#category : #'System-SessionManager-Tests'
}

{ #category : #running }
SessionManagerUnregistrationTest >> setUp [
	super setUp.
	sessionManager := SessionManager new
]

{ #category : #tests }
SessionManagerUnregistrationTest >> testCanUnregisterClass [
	sessionManager registerSystemClassNamed: #Foo.
	sessionManager registerSystemClassNamed: #Bar.

	sessionManager unregisterClassNamed: #Foo.
	
	self 
		assert: (sessionManager startupList collect: #handledId)
		equals: #( Bar ).
]

{ #category : #tests }
SessionManagerUnregistrationTest >> testUnregisterUnknownClassDoesNotFail [
	sessionManager registerSystemClassNamed: #Foo.

	sessionManager unregisterClassNamed: #Bar.
	
	self 
		assert: (sessionManager startupList collect: #handledId)
		equals: #( Foo ).
]
