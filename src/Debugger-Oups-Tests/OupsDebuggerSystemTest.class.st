Class {
	#name : #OupsDebuggerSystemTest,
	#superclass : #TestCase,
	#instVars : [
		'debuggerSystem'
	],
	#category : #'Debugger-Oups-Tests-Tests - infrastructure'
}

{ #category : #tests }
OupsDebuggerSystemTest >> createDummyDebugRequest [
	| exception |
	[ Exception signal ] on: Exception do: [ :e | exception := e ].
	^ OupsDummyDebugRequest newForException: exception
]

{ #category : #tests }
OupsDebuggerSystemTest >> createDummyDebugRequestForNotUIProcess [
	| exception nonUIProcess |
	nonUIProcess := [  1+1 ] newProcess.
	[ Exception signal ] on: Exception do: [ :e | exception := e ].
	^ (OupsDummyDebugRequest newForException: exception) process: nonUIProcess; yourself.
]

{ #category : #tests }
OupsDebuggerSystemTest >> createDummyDebugRequestForUIProcess [

	| exception |
	[ Exception signal ]
		on: Exception
		do: [ :e | exception := e ].
	^ (OupsDummyDebugRequest newForException: exception)
		  process: UIManager default uiProcess;
		  yourself
]

{ #category : #running }
OupsDebuggerSystemTest >> setUp [
	super setUp.
	
	"Put here a common initialization logic for tests"
	debuggerSystem := OupsDummyDebuggerSystem new.
]

{ #category : #tests }
OupsDebuggerSystemTest >> testAvailableDebuggers [
	"Tests that DebuggerSystem>>#availableDebuggers returns a list"
	self assert: debuggerSystem availableDebuggers isSequenceable
	
]

{ #category : #tests }
OupsDebuggerSystemTest >> testEnsureExceptionIn [
	|session|
	session := DebugSession new process: nil context: OupsDummyDebugger new.
	OupsDebuggerSystem basicNew ensureExceptionIn: session.
	self assert: session exception notNil.
	self assert: session exception class equals: OupsNullException.
	
	session := DebugSession new exception: #tag.
	OupsDebuggerSystem basicNew ensureExceptionIn: session.
	self assert: session exception equals: #tag
]

{ #category : #tests }
OupsDebuggerSystemTest >> testHandleDebugRequest [
	"Tests that a DebuggerSystem tasked to handle a DebugRequest calls the #handleDebugRequest: of its uiManager with that DebugRequest"
	| debugRequest |
	debuggerSystem customUIManager: OupsDummyUIManager new.
	debugRequest := OupsDebugRequest basicNew.
	self assert: debuggerSystem defaultUIManager callsToHandleDebugRequest equals: 0.
	debuggerSystem handleDebugRequest: debugRequest.
	self assert: debuggerSystem defaultUIManager callsToHandleDebugRequest equals: 1.
	self assert: debuggerSystem defaultUIManager debugRequestSentToHandleDebugRequest identicalTo: debugRequest
	
]

{ #category : #tests }
OupsDebuggerSystemTest >> testHandleWarningDebugRequest [
	"Tests that a DebuggerSystem tasked to handle a WarningDebugRequest calls the #handleWarningDebugRequest: of its uiManager with that DebugRequest"
	| debugRequest |
	debuggerSystem customUIManager: OupsDummyUIManager new.
	debugRequest := OupsDebugRequest basicNew.
	self assert: debuggerSystem defaultUIManager callsToHandleWarningDebugRequest equals: 0.
	debuggerSystem handleWarningDebugRequest: debugRequest.
	self assert: debuggerSystem defaultUIManager callsToHandleWarningDebugRequest equals: 1.
	self assert: debuggerSystem defaultUIManager debugRequestSentToHandleWarningDebugRequest identicalTo: debugRequest
	
]

{ #category : #tests }
OupsDebuggerSystemTest >> testNonUIProcessIsRecognised [
	"Tests that a DebuggerSystem opening a DebugRequest that is NOT on the ui process does NOT spawn a new UI process"
	debuggerSystem openDebuggerOnRequest: self createDummyDebugRequestForNotUIProcess.
	self assert: debuggerSystem newUIProcessSpawned not
	
]

{ #category : #tests }
OupsDebuggerSystemTest >> testPreDebugActionExecuted [
	"Test that a DebuggerSystem opening a DebugRequest with a pre-debug action will execute it exactly once"
	| debugRequest |
	debugRequest := self createDummyDebugRequest.
	self assert: debugRequest dummyPreDebugActionExecutedCount equals: 0.
	debuggerSystem openDebuggerOnRequest: debugRequest.
	self assert: debugRequest dummyPreDebugActionExecutedCount equals: 1
	
]

{ #category : #tests }
OupsDebuggerSystemTest >> testUIProcessIsRecognised [

	"Test that a DebuggerSystem opening a DebugRequest on the ui process will spawn a new UI process"

	debuggerSystem spawnNewUIProcessIfNecessary:
		self createDummyDebugRequestForUIProcess.
	self assert: debuggerSystem newUIProcessSpawned
]
