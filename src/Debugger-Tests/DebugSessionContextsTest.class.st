Class {
	#name : #DebugSessionContextsTest,
	#superclass : #DebuggerTest,
	#instVars : [
		'debuggedThisContext'
	],
	#category : #'Debugger-Tests'
}

{ #category : #running }
DebugSessionContextsTest >> setUp [
	"Creates a debugSession in a way that mimicks the real way DebugSessions are created normally in the image (in a process, a halt is raised, the process creates a debugSession and gets suspended). Note that this isn't a perfect replica of the normal way a debugSession gets created, because the normal way involves the UIManager and would create a pre-debug window."

	super setUp.
	process := [ | debugSession |
	debugSession := Processor activeProcess newDebugSessionNamed: 'test' startedAt: (debuggedThisContext := thisContext).
	Processor activeProcess suspend ] fork.
	"The line under is a bit dirty. It is to make sure that the debugged process has suspended itself before it is being examined by the test methods."
	[ process isSuspended ] whileFalse: [ 100 milliSeconds wait ]
]

{ #category : #tests }
DebugSessionContextsTest >> testInitialContentOfInterruptedContext [
	"Checks that the interruptedContext of the DebugSession is the thisContext of the code where the DebugSession was created"
	self assert: ((process suspendedContext lookupSymbol: #debugSession) interruptedContext ) identicalTo: debuggedThisContext.
]

{ #category : #tests }
DebugSessionContextsTest >> testInitialContentOfSuspendedContext [
	"Checks that the interruptedContext of the DebugSession is the thisContext of the code where the DebugSession was created"
	self assert: ((process suspendedContext lookupSymbol: #debugSession) interruptedProcess suspendedContext) identicalTo: debuggedThisContext.
]
