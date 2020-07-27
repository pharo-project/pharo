"
Utilitary class to provide various kind of debuggers, interrupted context and debug sessions for testing purposes
"
Class {
	#name : #StTestDebuggerProvider,
	#superclass : #Object,
	#instVars : [
		'session'
	],
	#category : #'NewTools-Debugger-Tests-Utils'
}

{ #category : #helpers }
StTestDebuggerProvider class >> compileMissingClassContextBuilder [
	
	self compile: 'buildDebuggerWithMissingClassContext
	
	[ MissingClass new ]
		on: Error
		do: [ :err | 			
			self sessionFor: err signalerContext copy.
			^ self newDebugger ]'
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithContext: aContext [
	self sessionFor: aContext.
	^ self newDebugger
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithDNUContext [
	[ Object new toto ]
		on: Error
		do: [ :err | 
			self sessionFor: err signalerContext.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithErrorContext [
	[ #() at: 1 ]
		on: Error
		do: [ :err | 
			self sessionFor: err signalerContext copy.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithFailingAssertionContext [
	[ self produceAssertionFailure ]
		on: TestFailure
		do: [ :err | 
			self sessionFor: err signalerContext copy.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithFailingTestContext [
	[ self produceTestFailure ]
		on: TestFailure
		do: [ :err | 
			self sessionFor: err signalerContext copy.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithMissingClassContext [
	"The called method must contain a undeclared variable (the missing class).
	It makes the CI test fail, so it has to be generated and removed after usage.
	buildDebuggerWithMissingClassContext has to be generated when starting a test (see class side for generation) and removed in the test's tearDown."
	^ self buildDebuggerWithMissingClassContext
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithMissingSubclassResponsibilityContext [
	[ StDummyDebuggerPresenter new unimplementedSubclassResponsibility ]
		on: Error
		do: [ :err | 
			self sessionFor: err signalerContext.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithRunnableContext [
	[ Halt now ]
		on: Halt
		do: [ :err | 
			self sessionFor: err signalerContext.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> newDebugger [
	^ StDebugger basicNew
		session: session;
		yourself
]

{ #category : #helpers }
StTestDebuggerProvider >> produceAssertionFailure [
	TestCase new assert: 4 equals: 2
]

{ #category : #helpers }
StTestDebuggerProvider >> produceTestFailure [
	TestCase new fail
]

{ #category : #helpers }
StTestDebuggerProvider >> sessionFor: aContext [
	| process |
	process := Process
		forContext: aContext
		priority: Processor userInterruptPriority.
	session := DebugSession
		named: 'test session'
		on: process
		startedAt: aContext
]

{ #category : #helpers }
StTestDebuggerProvider >> terminate [ 
	session ifNotNil:[session terminate].
	session := nil
]
