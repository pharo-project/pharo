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
DebuggerSelectorTest >> setUp [
	"Hooks that subclasses may override to define the fixture of test."
	super setUp.
"	debuggerSelector := DebuggerSelector new.
	debuggerSelector := DebuggerSelectorDecoratorForTest newOn: debuggerSelector."
	debuggerSelector := DebuggerSelectorForTests new.
	debugSession := DebugSession new.
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
	debuggerSelector mainDebuggerClass: DummyDebuggerForTests_RankZero.
	debuggerSelector selectDebuggerAndOpen: debugSession withFullView: nil andNotification: nil.
	self assert: ((debuggerSelector debuggerSelectionAttempts at: 1) debuggerClass) equals: DummyDebuggerForTests_RankZero
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
