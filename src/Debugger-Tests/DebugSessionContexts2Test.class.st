Class {
	#name : #DebugSessionContexts2Test,
	#superclass : #DebuggerTest,
	#classVars : [
		'debugSession'
	],
	#category : #'Debugger-Tests'
}

{ #category : #'as yet unclassified' }
DebugSessionContexts2Test class >> debugSession [
	^ debugSession.
]

{ #category : #'instance creation' }
DebugSessionContexts2Test class >> debugSession: aValue [
	debugSession := aValue
]

{ #category : #running }
DebugSessionContexts2Test >> setUp [
	"Creates a debugSession in a way that mimicks the real way DebugSessions are created normally in the image (in a process, a halt is raised, the process creates a debugSession and gets suspended). Note that this isn't a perfect replica of the normal way a debugSession gets created, because the normal way involves the UIManager and would create a pre-debug window."
	super setUp.
	process := [ 1 + 1.
	MyHalt signal ] fork.
	"The line under is a bit dirty. It is to make sure that the debugged process has suspended itself before it is being examined by the test methods."
	[process isSuspended] whileFalse: [ 100 milliSeconds wait. ].
	session := self class debugSession.
]

{ #category : #tests }
DebugSessionContexts2Test >> testContextsAfterStepInto [
	"Checks that after one step into of the debug session, the suspendedContext of its interruptedProcess is at the user's code (not in the debugging machinery) and is the same object as the interruptedContext of the DebugSession."

	session stepInto.
	self
		assert: session interruptedProcess suspendedContext method
		equals: self class >> #setUp.
	self
		assert: session interruptedProcess suspendedContext
		identicalTo: session interruptedContext
]

{ #category : #tests }
DebugSessionContexts2Test >> testInterruptedContext [
	"Checks that the interruptedContext of the debug session is the last context the execution went through that contains the user's code and not the debugging machinery."
	self assert: (session interruptedContext method) equals: (self class)>>#setUp.
]

{ #category : #tests }
DebugSessionContexts2Test >> testSuspendedContext [
	"Checks that the suspendedContext of the interruptedProcess of a DebugSession is the last context the execution was at when its process was suspended."
	self assert: (session interruptedProcess suspendedContext method) equals: MyHalt>>#defaultAction.
]
