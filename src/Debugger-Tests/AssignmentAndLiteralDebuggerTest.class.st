Class {
	#name : #AssignmentAndLiteralDebuggerTest,
	#superclass : #DebuggerTest,
	#category : #'Debugger-Tests'
}

{ #category : #running }
AssignmentAndLiteralDebuggerTest >> setUp [
	super setUp.
	self settingUpSessionAndProcessAndContextForBlock: [ |var| var:=4].
]

{ #category : #tests }
AssignmentAndLiteralDebuggerTest >> testInterruptedContextEqualsSuspendedContextOfInterruptedProcess [
	self assert: session interruptedContext equals: session interruptedProcess suspendedContext.
	session stepInto.
	session stepInto.
	self assert: session interruptedContext equals: session interruptedProcess suspendedContext.
]

{ #category : #tests }
AssignmentAndLiteralDebuggerTest >> testNewDebugSession [
	self assert: context size equals: 0.
	self assert: session interruptedContext equals: context.
]

{ #category : #tests }
AssignmentAndLiteralDebuggerTest >> testStepIntoAssignment [
	session stepInto.
	session stepInto.
	self assert: (context tempNamed: 'var') equals: 4.
]

{ #category : #tests }
AssignmentAndLiteralDebuggerTest >> testStepIntoLiteral [
	session stepInto.
	self assert: context top equals: 4.
]

{ #category : #tests }
AssignmentAndLiteralDebuggerTest >> testStepOverAssignment [
	session stepOver.
	session stepOver.
	self assert: (context tempNamed: 'var') equals: 4.
]

{ #category : #tests }
AssignmentAndLiteralDebuggerTest >> testStepOverLiteral [
	session stepOver.
	self assert: context top equals: 4.
]
