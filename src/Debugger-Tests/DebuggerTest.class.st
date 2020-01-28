"
Superclass for classes containing debugger tests.
Provides the ability to send #debuggerOnSetUpSession to a debugger test class to obtain a debugger opened on the debug session set up by that class for its tests.
"
Class {
	#name : #DebuggerTest,
	#superclass : #TestCase,
	#instVars : [
		'context',
		'session',
		'process'
	],
	#category : #'Debugger-Tests'
}

{ #category : #utilities }
DebuggerTest >> inspectCurrentNode: aDebugSession [
	context := aDebugSession interruptedContext.
	(context method sourceNodeForPC: context pc) inspect.
]

{ #category : #accessing }
DebuggerTest >> session [
	^ session
]

{ #category : #running }
DebuggerTest >> setUp [
	super setUp.
	context := nil.
	process := nil.
	session := nil.
]

{ #category : #utilities }
DebuggerTest >> settingUpSessionAndProcessAndContextForBlock: aBlock [
	process := Process
		forContext: 
			[aBlock value.
			Processor terminateActive] asContext
		priority: Processor userInterruptPriority.
	[process step closure == aBlock] whileFalse.
	process step.
	context := process suspendedContext.
	session:= process newDebugSessionNamed: 'test session' startedAt: context.
]
