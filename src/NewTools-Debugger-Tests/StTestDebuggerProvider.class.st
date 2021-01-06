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
			self sessionFor: err signalerContext copy exception: err.
			^ self newDebugger ]'
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithContext: aContext [
	self sessionFor: aContext.
	^ self newDebugger
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithDNUContext [
	[ self methodProducingDNU ]
		on: Error
		do: [ :err | 
			self sessionFor: err signalerContext exception: err.					
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithErrorContext [

	[ #(  ) at: 1 ]
		on: Error
		do: [ :err | 
			self sessionFor: err signalerContext exception: err.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithFailingAssertionContext [
	[ self produceAssertionFailure ]
		on: TestFailure
		do: [ :err | 
			self sessionFor: err signalerContext exception: err.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithFailingTestContext [
	[ self produceTestFailure ]
		on: TestFailure
		do: [ :err | 
			self sessionFor: err signalerContext exception: err.
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
		on: SubclassResponsibility
		do: [ :err | 			
			self sessionFor: err signalerContext exception: err.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithObjectHalting [
	[ StDebuggerObjectForTests new haltingMethod ]
		on: Halt
		do: [ :err | 
			self sessionFor: err signalerContext exception: err.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> debuggerWithRunnableContext [
	[ Halt now ]
		on: Halt
		do: [ :err | 
			self sessionFor: err signalerContext exception: err.
			^ self newDebugger ]
]

{ #category : #helpers }
StTestDebuggerProvider >> methodProducingDNU [

	^ self foobar
]

{ #category : #helpers }
StTestDebuggerProvider >> newDebugger [

	| debugger |
	debugger := StDebugger basicNew.
	debugger session: session.
	debugger debuggerActionModel updateContextPredicate.
	^ debugger
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
StTestDebuggerProvider >> session [

	^ session
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
		           startedAt: aContext.
	session exception ifNil: [ session exception: OupsNullException new ]
]

{ #category : #helpers }
StTestDebuggerProvider >> sessionFor: aContext exception: e [

	e freezeUpTo: nil.
	self sessionFor: e signalerContext.
	session exception: e
]

{ #category : #helpers }
StTestDebuggerProvider >> sessionForDebuggerTests [

	| context process sessionForTests |
	context := [ 
	           | i |
	           i := 1 squared.
	           i = 1 ifTrue: [ 
		           | j |
		           j := i + 1 ].
	           ^ 1 + i ] asContext.
	process := Process
		           forContext: context
		           priority: Processor userInterruptPriority.
	sessionForTests := DebugSession
		                   named: 'test session'
		                   on: process
		                   startedAt: context.
	sessionForTests exception: (OupsNullException fromSignallerContext: context).
	^ sessionForTests
]

{ #category : #helpers }
StTestDebuggerProvider >> sessionWithDNUAfterStep [

	| context process sessionForTests |
	context := [ ^ self methodProducingDNU ] asContext.
	process := Process
		           forContext: context
		           priority: Processor userInterruptPriority.
	sessionForTests := DebugSession
		                   named: 'test session'
		                   on: process
		                   startedAt: context.
	sessionForTests exception: (OupsNullException fromSignallerContext: context).
	^ sessionForTests
]

{ #category : #helpers }
StTestDebuggerProvider >> terminate [ 
	session ifNotNil:[session clear].
	session := nil
]
