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
	#category : #'Debugger-Model-Tests-Model'
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
DebuggerModelTest >> testClearDebugSession [

	session clear.
	self assert: session exception equals: nil.
	self assert: session interruptedContext equals: nil.
	self assert: session interruptedProcess equals: nil
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

{ #category : #tests }
DebuggerModelTest >> testUnwindAndRestartToContext [
	
	context := [ 2 squared ] asContext.
	process := Process 
		forContext: context 
		priority: Processor userInterruptPriority.
		
	session:= process newDebugSessionNamed: 'test session' startedAt: context.
	
	context := context step.
	context := context step.
	process suspendedContext: context.
	session updateContextTo: context.		
	
	session unwindAndRestartToContext: session interruptedContext.
	self assert: session interruptedContext method equals: (Number >> #squared).
	"After restarting a context, we step to the first interesting bytecode, that is either a send or a return.
	In the (Number >> #squared) method, the first send is the third bytecode.
	So, after restarting, the context's pc should be equal to the initialPC (first bytecode) + 2 (third bytecode = send)"
	self assert: session interruptedContext pc equals: (Number >> #squared) initialPC + 2 
]

{ #category : #tests }
DebuggerModelTest >> testUnwindAndRestartToQuickMethodContext [
	
	context := [ #foo asSymbol ] asContext.
	process := Process 
		forContext: context 
		priority: Processor userInterruptPriority.
		
	session:= process newDebugSessionNamed: 'test session' startedAt: context.
	
	context := context step.			
	context stepIntoQuickMethod: true.
	context := context step.
	process suspendedContext: context.
	session updateContextTo: context.	
	
	session unwindAndRestartToContext: session interruptedContext.
	self assert: session interruptedContext method equals: (Symbol >> #asSymbol).
	self assert: session interruptedContext pc equals: (Symbol >> #asSymbol) endPC 
]
