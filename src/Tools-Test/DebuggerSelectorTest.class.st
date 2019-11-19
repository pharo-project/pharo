Class {
	#name : #DebuggerSelectorTest,
	#superclass : #TestCase,
	#instVars : [
		'debuggerSelector',
		'eventListener',
		'debugSession',
		'testDebuggerClass'
	],
	#category : #'Tools-Test-Debugger'
}

{ #category : #running }
DebuggerSelectorTest >> createDebugSessionForTesting [
	| dbSession process |
	process := [ 'hello'] fork.
	dbSession :=  process newDebugSessionNamed: 'forTest' startedAt: process suspendedContext.
	^ dbSession
]

{ #category : #running }
DebuggerSelectorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
"	debuggerSelector := DebuggerSelector new.
	debuggerSelector := DebuggerSelectorDecoratorForTest newOn: debuggerSelector."
	debuggerSelector := DebuggerSelectorForTests new.
	"Adding the test debugger classes the tests will work with"
	debuggerSelector debuggerClasses add: DummyDebuggerForTests; add: DummyDebuggerForTests_CannotOpenDebugSession; add: DummyDebuggerForTests_RankZero.
	debugSession := self createDebugSessionForTesting.
	eventListener := EventListenerForDebuggerSelectorTest new.
	debuggerSelector addSubscriberOnMainDebuggerClassUpdate: eventListener.
	testDebuggerClass := DummyDebuggerForTests.
]

{ #category : #tests }
DebuggerSelectorTest >> testIsThisClassADebugger [
	self assert: (debuggerSelector isThisClassADebugger: DummyDebuggerForTests).
	self assert: (debuggerSelector isThisClassADebugger: DummyDebuggerForTests_CannotOpenDebugSession).
	self assert: (debuggerSelector isThisClassADebugger: self class) not.
]

{ #category : #tests }
DebuggerSelectorTest >> testMainDebuggerAlgorithm [
	"Tests that the first debugger that DebuggerSelector will try to open is the one that has been set as main debugger, even if another debugger has a higher rank"
	debuggerSelector mainDebuggerClass: DummyDebuggerForTests_RankZero.
	debuggerSelector selectDebuggerAndOpen: debugSession withFullView: nil andNotification: nil.
	self assert: (debuggerSelector debuggerSelectionAttempts size) equals: 1.
	self assert: ((debuggerSelector debuggerSelectionAttempts at: 1) debuggerClass) equals: DummyDebuggerForTests_RankZero
]

{ #category : #tests }
DebuggerSelectorTest >> testMainDebuggerAlgorithm_MainDebuggerCannotOpen [
	"Tests that the first debugger that DebuggerSelector will try to open is the one that has been set as main debugger, even if another debugger has a higher rank"
	debuggerSelector mainDebuggerClass: DummyDebuggerForTests_CannotOpenDebugSession.
	debuggerSelector selectDebuggerAndOpen: debugSession withFullView: nil andNotification: nil.
	self assert: (debuggerSelector debuggerSelectionAttempts size) equals: 3.
	"Assert the debuggerSelector tried to open the main debugger first, but failed (as this particular debugger fails to open any debug session)"
	self assert: ((debuggerSelector debuggerSelectionAttempts at: 1) debuggerClass) equals: DummyDebuggerForTests_CannotOpenDebugSession.
	self assert: (debuggerSelector debuggerSelectionAttempts at: 1) success not. 
	"Assert that after failing to open the main debugger, the debuggerSelector reverted to the standard algorithm of trying to open the highest ranked debugger first. The highest ranked debugger happen to be DummyDebuggerForTests_CannotOpenDebugSession"
	self assert: ((debuggerSelector debuggerSelectionAttempts at: 2) debuggerClass) equals: DummyDebuggerForTests_CannotOpenDebugSession.
	"Assert that the next debugger tried could open the debug session"
	self assert: (debuggerSelector debuggerSelectionAttempts at: 3) debuggerClass equals: DummyDebuggerForTests.
	self assert: (debuggerSelector debuggerSelectionAttempts at: 3) success.
]

{ #category : #tests }
DebuggerSelectorTest >> testMainDebuggerSetGet [
	"Test that setter methods #mainDebuggerClass: and #setNoMainDebuggerClass correctly set the main debugger class"
	debuggerSelector mainDebuggerClass: DummyDebuggerForTests_RankZero.
	self assert: debuggerSelector mainDebuggerClass equals: DummyDebuggerForTests_RankZero.
	debuggerSelector setNoMainDebuggerClass.
	self assert: debuggerSelector mainDebuggerClass equals: nil
]

{ #category : #tests }
DebuggerSelectorTest >> testSettingNonDebuggerAsMainDebuggerSignalsException [
	"Setting as main debugger a class that is not a debugger should signal a CannotSetAsMainDebuggerAClassThatIsNotADebuggerClass exception"
	self should: [debuggerSelector mainDebuggerClass: Object] raise: CannotSetAsMainDebuggerAClassThatIsNotADebuggerClass.
]

{ #category : #tests }
DebuggerSelectorTest >> testTryToOpenDebugSession [
	"The purpose of this test is to check that the instrumentation of #tryToOpenDebugSession:withDebuggerClass:withFullView:andNotification: on DebuggerSelectorDecoratorForTest works properly"
	debuggerSelector tryToOpenDebugSession: debugSession withDebuggerClass: testDebuggerClass withFullView: nil andNotification: nil.
	self assert: debuggerSelector debuggerSelectionAttempts size equals: 1.
	self assert: (debuggerSelector debuggerSelectionAttempts at: 1) success.
	self assert: (debuggerSelector debuggerSelectionAttempts at: 1) debuggerClass equals: testDebuggerClass.
]
