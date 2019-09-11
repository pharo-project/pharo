Class {
	#name : #ErrorWasInUIProcessTest,
	#superclass : #DebuggerTest,
	#category : #'Debugger-Tests'
}

{ #category : #tests }
ErrorWasInUIProcessTest >> testErrorWasInUIProcessIsFalseWhenDebugSessionWasCreatedByANewProcess [
	"Test that errorWasInUIProcess is false if the DebugSession was created by a new process"
	self settingUpSessionAndProcessAndContextForBlock: [ 1+1].
	self assert: (session errorWasInUIProcess not)
]

{ #category : #tests }
ErrorWasInUIProcessTest >> testErrorWasInUIProcessIsTrueWhenDebugSessionWasCreatedByTheUIProcess [
	"Test that errorWasInUIProcess is true when the DebugSession was created by the UI process"
	
	self skipOnPharoCITestingEnvironment.
	SmalltalkImage current isHeadless ifFalse: [ ^ true ].
	context := [ 1+1] asContext.
	"Instead of creating a new process like the other debugger tests, we ask the default ui process to create the DebugSession"
	session := UIManager default uiProcess newDebugSessionNamed: 'test session' startedAt: context.
	self assert: (session errorWasInUIProcess)
]
