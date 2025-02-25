Class {
	#name : 'OupsDebuggerSystemTest',
	#superclass : 'TestCase',
	#instVars : [
		'debuggerSystem'
	],
	#category : 'Debugger-Oups-Tests-Tests - infrastructure',
	#package : 'Debugger-Oups-Tests',
	#tag : 'Tests - infrastructure'
}

{ #category : 'tests' }
OupsDebuggerSystemTest >> createDummyDebugRequest [
	| exception |
	[ Exception signal ] on: Exception do: [ :e | exception := e ].
	^ OupsDummyDebugRequest newForException: exception
]

{ #category : 'private' }
OupsDebuggerSystemTest >> performTest [
	"Temporary printing to find more information about https://github.com/pharo-project/pharo/issues/12502"

	('Running: ' , testSelector asString , ' from ' , self class name) traceCr.
	self perform: testSelector asSymbol
]

{ #category : 'running' }
OupsDebuggerSystemTest >> setUp [
	super setUp.

	"Put here a common initialization logic for tests"
	debuggerSystem := OupsDummyDebuggerSystem new
]

{ #category : 'tests' }
OupsDebuggerSystemTest >> testAvailableDebuggers [
	"Tests that DebuggerSystem>>#availableDebuggers returns a list"
	self assert: debuggerSystem availableDebuggers isSequenceable
]

{ #category : 'tests' }
OupsDebuggerSystemTest >> testEnsureExceptionIn [
	|session|
	session := DebugSession new process: nil context: OupsDummyDebugger new.
	OupsDebuggerSystem basicNew ensureExceptionIn: session.
	self assert: session exception isNotNil.
	self assert: session exception class equals: OupsNullException.

	session := DebugSession new exception: #tag.
	OupsDebuggerSystem basicNew ensureExceptionIn: session.
	self assert: session exception equals: #tag
]

{ #category : 'tests' }
OupsDebuggerSystemTest >> testHandleDebugRequest [
	"Tests that a DebuggerSystem tasked to handle a DebugRequest calls the #handleDebugRequest: of its uiManager with that DebugRequest"
	| debugRequest |
	debugRequest := self createDummyDebugRequest.
	self assert: debuggerSystem callsToHandleDebugRequest equals: 0.
	debuggerSystem handleDebugRequest: debugRequest.
	self assert: debuggerSystem callsToHandleDebugRequest equals: 1.
	self assert: debuggerSystem debugRequestSentToHandleDebugRequest identicalTo: debugRequest
]

{ #category : 'tests' }
OupsDebuggerSystemTest >> testHandleWarningDebugRequest [
	"Tests that a DebuggerSystem tasked to handle a WarningDebugRequest calls the #handleWarningDebugRequest: of its uiManager with that DebugRequest"
	| debugRequest |
	debugRequest := self createDummyDebugRequest.
	self assert: debuggerSystem callsToHandleDebugRequest equals: 0.
	debuggerSystem handleWarningDebugRequest: debugRequest.
	self assert: debuggerSystem callsToHandleDebugRequest equals: 1.
	self assert: debuggerSystem debugRequestSentToHandleDebugRequest identicalTo: debugRequest
]

{ #category : 'tests' }
OupsDebuggerSystemTest >> testPreDebugActionExecuted [
	"Test that a DebuggerSystem opening a DebugRequest with a pre-debug action will execute it exactly once"
	| debugRequest |
	debugRequest := self createDummyDebugRequest.
	self assert: debugRequest dummyPreDebugActionExecutedCount equals: 0.
	debuggerSystem openDebuggerOnRequest: debugRequest.
	self assert: debugRequest dummyPreDebugActionExecutedCount equals: 1
]
