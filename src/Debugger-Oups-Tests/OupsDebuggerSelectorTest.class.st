Class {
	#name : #OupsDebuggerSelectorTest,
	#superclass : #TestCase,
	#instVars : [
		'debuggers',
		'debuggerSelector'
	],
	#category : #'Debugger-Oups-Tests-tests - strategies'
}

{ #category : #helper }
OupsDebuggerSelectorTest >> session [
	^OupsDummyDebugger dummySession
]

{ #category : #running }
OupsDebuggerSelectorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."

	super setUp.
	debuggers := OupsDummyDebugger dummyDebuggerList.
	debuggerSelector := OupsDebuggerSelector with: debuggers copy.
	debuggerSelector handlesDebuggerErrors: false
]

{ #category : #running }
OupsDebuggerSelectorTest >> tearDown [
	debuggers
		do: [ :dbg | 
			(dbg tag isKindOf: DebugSession)
				ifTrue: [ dbg tag terminate ] ].
	super tearDown
]

{ #category : #'tests - debuggers' }
OupsDebuggerSelectorTest >> testDebuggers [
	self
		assertCollection: debuggerSelector debuggers asOrderedCollection
		equals: debuggers
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testFirstUsableDebuggerForSession [
	
	debuggerSelector debuggers first beUnusable.
	debuggerSelector debuggers second beUnusable.

	self
		assert: (debuggerSelector nextDebuggerForSession: self session)
		identicalTo: debuggers third
	
	
]

{ #category : #'tests - debugging' }
OupsDebuggerSelectorTest >> testHandlesDebuggerErrors [
	self deny: debuggerSelector handlesDebuggerErrors.
	debuggerSelector handlesDebuggerErrors: true.
	self assert: debuggerSelector handlesDebuggerErrors.
	self
		assert: OupsDebuggerSelector new handlesDebuggerErrors
		equals: OupsDebuggerSelector handleDebuggerErrors
]

{ #category : #'tests - debuggers' }
OupsDebuggerSelectorTest >> testNextDebugger [
	debuggers
		do: [ :dbg | self assert: debuggerSelector nextDebugger identicalTo: dbg ]
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testNextDebuggerForDebuggerFailure [
	"Upon a debugger failure, we call the nextDebuggerForSession: interface to get the next debugger"
	|session|
	session := self session.
	session exception: (OupsDebuggerFailure of: debuggers first on: session exception).
	self
		assert: (debuggerSelector nextDebuggerForSession: session)
		identicalTo: debuggers second

	
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testNextDebuggerForSession [
	"Upon a debugger failure, we call the nextDebuggerForSession: interface to get the next debugger"
	
	self
		assert: (debuggerSelector nextDebuggerForSession: self session)
		identicalTo: debuggers first.
	
	self
		assert: (debuggerSelector nextDebuggerForSession: self session)
		identicalTo: debuggers second.
		
	self
		assert: (debuggerSelector nextDebuggerForSession: self session)
		identicalTo: debuggers last.
		
	self
		should: [ debuggerSelector nextDebuggerForSession: self session]
		raise: CollectionIsEmpty
		
		

]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testNoDebuggerForSession [
	debuggerSelector debuggers: #().
	self
		shouldnt: [ debuggerSelector openDebuggerForSession: self session ]
		raise: CollectionIsEmpty.
	self deny: debuggerSelector handled
]

{ #category : #'tests - debuggers' }
OupsDebuggerSelectorTest >> testNoNextDebugger [
	debuggerSelector debuggers: #().
	self
		should: [ debuggerSelector nextDebugger ]
		raise: CollectionIsEmpty
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testOpenDebuggerError [
	| session |
	session := self session.
	session exception: (OupsDebuggerFailure of: debuggers first on: session exception).
	debuggers first beError.
	debuggers second beError.
	
	"Because we are debugging a debugger error, we cannot raise another DebuggerFailure:
	we want to find a debugger to debug the failing debugger."
	debuggerSelector handlesDebuggerErrors: true.	
	self shouldnt: [debuggerSelector openDebuggerForSession: session] raise: OupsDebuggerFailure.	
		
	"Because the first debugger originated the debugger failure,
	it is not considered in the algorithm anymore."
	self assert: debuggers first tag equals: nil.
	self assert: debuggers second tag class identicalTo: Error.
	self assert: debuggers third tag identicalTo: session.
	self assert: debuggerSelector handled
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testOpenDebuggerForSession [
	| session |
	session := self session.
	debuggerSelector openDebuggerForSession: session.
	self assert: debuggers first tag identicalTo: session.
	self assert: debuggerSelector handled
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testOpenDebuggerWithErrorForSession [
	| session |
	session := self session.
	debuggers first beError.
	debuggers second beError.			
	debuggerSelector handlesDebuggerErrors: false.
	debuggerSelector openDebuggerForSession: session.
	self assert: debuggers first tag class identicalTo: Error.
	self assert: debuggers second tag class identicalTo: Error.	
	self assert: debuggers third tag identicalTo: session.
	self assert: debuggerSelector handled.
	self assert: debuggerSelector hasErrors.
	self assert: debuggerSelector error class identicalTo: Error
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testTryOpenWith [
	| session |
	session := self session.
	self
		shouldnt: [ debuggerSelector tryOpen: session with: debuggers first ]
		raise: OupsDebuggerFailure.
	self assert: debuggers first tag identicalTo: session.
	self assert: debuggerSelector handled
]

{ #category : #'tests -  algorithm' }
OupsDebuggerSelectorTest >> testTryOpenWithSignalsError [
	| session |
	session := self session.
	debuggers first beError.
	debuggers second beError.
	
	self
		shouldnt: [ debuggerSelector tryOpen: session with: debuggers first ]
		raise: OupsDebuggerFailure.
	self deny: debuggerSelector handled.	

	debuggerSelector handlesDebuggerErrors: true.
	self
		should: [ debuggerSelector tryOpen: session with: debuggers second ]
		raise: OupsDebuggerFailure.
	self deny: debuggerSelector handled
]
