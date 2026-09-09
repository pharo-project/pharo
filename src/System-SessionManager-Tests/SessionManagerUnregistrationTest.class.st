"
SUnit tests for unregistration from the Pharo session manager
"
Class {
	#name : 'SessionManagerUnregistrationTest',
	#superclass : 'TestCase',
	#instVars : [
		'sessionManager'
	],
	#category : 'System-SessionManager-Tests',
	#package : 'System-SessionManager-Tests'
}

{ #category : 'running' }
SessionManagerUnregistrationTest >> setUp [
	super setUp.
	sessionManager := SessionManager new
]

{ #category : 'tests' }
SessionManagerUnregistrationTest >> testCanUnregisterClass [

	sessionManager registerSystemClassNamed: #Foo.
	sessionManager registerSystemClassNamed: #Bar.
	sessionManager unregisterClassNamed: #Foo.
	self
		assert: (sessionManager startupList collect: [:each | each handlerId])
		equals: #( Bar )
]

{ #category : 'tests' }
SessionManagerUnregistrationTest >> testUnregisterUnknownClass [

	| absent |
	absent := false.
	sessionManager registerSystemClassNamed: #Foo.
	sessionManager unregisterHandler: #Bar ifAbsent: [ absent := true ].
	
	self assert: absent
]
