"
SUnit tests for session creation with SessionManager
"
Class {
	#name : #SessionCreationTest,
	#superclass : #TestCase,
	#category : #'System-SessionManager-Tests'
}

{ #category : #tests }
SessionCreationTest >> testNewSession [
	| manager session |
	manager := SessionManager new.
	session := manager newSession.
	session stop: false.
]

{ #category : #tests }
SessionCreationTest >> testShutdown [
	| manager session executed |
	executed := false.
	manager := SessionManager new.
	
	manager register: (TestSessionHandler onShutdown: [ executed := true ]).
	
	session := manager newSession.
	self deny: executed.
	
	session stop: false.
	self assert: executed.
]

{ #category : #tests }
SessionCreationTest >> testStartup [
	| manager session executed |
	executed := false.
	manager := SessionManager new.
	
	manager register: (TestSessionHandler onStartup: [ executed := true ]).
	
	session := manager newSession.
	session start: false.
	
	self assert: executed
]
