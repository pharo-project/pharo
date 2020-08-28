Class {
	#name : #OupsSingleDebuggerSelectorTest,
	#superclass : #TestCase,
	#instVars : [
		'debuggers',
		'debuggerSelector'
	],
	#category : #'Debugger-Oups-Tests-Tests - strategies'
}

{ #category : #running }
OupsSingleDebuggerSelectorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	super setUp.
	debuggers := OupsDummyDebugger dummyDebuggerList.
	debuggerSelector := OupsSingleDebuggerSelector with: debuggers copy
]

{ #category : #running }
OupsSingleDebuggerSelectorTest >> tearDown [
	debuggers
		do: [ :dbg | 
			(dbg tag isKindOf: DebugSession)
				ifTrue: [ dbg tag terminate ] ].
	super tearDown
]

{ #category : #tests }
OupsSingleDebuggerSelectorTest >> testError [
	self deny: OupsDebuggerSelector new hasErrors
]

{ #category : #tests }
OupsSingleDebuggerSelectorTest >> testErrorWhenOpeningDebugger [
	| session |
	session := OupsDummyDebugger dummySession.
	debuggers first beError.	
	debuggerSelector openDebuggerForSession: session.
	
	self assert: debuggerSelector hasErrors.
	self assert: debuggerSelector error class identicalTo: Error.
	self deny: debuggerSelector handled
	
]

{ #category : #tests }
OupsSingleDebuggerSelectorTest >> testHandled [
	| session |
	session := OupsDummyDebugger dummySession.
	debuggers first beError.
	
	self assert: debuggerSelector handled.
	
	debuggerSelector openDebuggerForSession: session.
	self deny: debuggerSelector handled
]

{ #category : #tests }
OupsSingleDebuggerSelectorTest >> testNextDebugger [
	self
		assert: debuggerSelector nextDebugger
		identicalTo: debuggers first.
	self
		assert: debuggerSelector nextDebugger
		identicalTo: debuggers first.
	self
		assert: debuggerSelector nextDebugger
		identicalTo: debuggers first
]

{ #category : #tests }
OupsSingleDebuggerSelectorTest >> testOpenDebuggerForSession [
	| session |
	session := OupsDummyDebugger dummySession.
	
	debuggerSelector openDebuggerForSession: session.
	self assert: debuggers first tag identicalTo: session.	
	
	debuggers first tag: nil.
	debuggerSelector openDebuggerForSession: session.
	self assert: debuggers first tag identicalTo: session.	
	self assert: debuggers second tag equals: nil.
	
	debuggers first tag: nil.
	debuggerSelector openDebuggerForSession: session.
	self assert: debuggers first tag identicalTo: session.	
	self assert: debuggers third tag equals: nil.
]
