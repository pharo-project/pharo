"
SUnit tests for the debugger model
"
Class {
	#name : #DebuggerModelTest,
	#superclass : #TestCase,
	#instVars : [
		'session',
		'process',
		'context'
	],
	#category : #'Debugger-Tests'
}

{ #category : #running }
DebuggerModelTest >> setUp [
	super setUp.
	context := [ Set new ] asContext.
	process := Process 
		forContext: context 
		priority: Processor userInterruptPriority.
		
	session:= process newDebugSessionNamed: 'test session' startedAt: context.
]

{ #category : #tests }
DebuggerModelTest >> testCorrectlyCreateDebugSession [

	self assert: session isNotNil.	
	self assert: process isSuspended.
	self assert: session interruptedContext equals: context.
	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp'.
]

{ #category : #tests }
DebuggerModelTest >> testStepInto [

	self deny: (session isContextPostMortem: context).
	self assert: session interruptedContext equals: context.
	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp' .

	session 
		stepInto;
		stepInto.

	self assert: session interruptedContext printString equals: 'Set class(HashedCollection class)>>new'.

]

{ #category : #tests }
DebuggerModelTest >> testStepOver [

	self assert: session interruptedContext equals: context.
	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp' .

	session stepOver.

	self assert: session interruptedContext printString equals: '[ Set new ] in DebuggerModelTest>>setUp'.
]
