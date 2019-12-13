Class {
	#name : #ContextDebuggingTest,
	#superclass : #TestCase,
	#category : #'Debugging-Utils-Tests'
}

{ #category : #tests }
ContextDebuggingTest >> deadContext [
	| process context |
	process := [  ] newProcess.
	context := process suspendedContext.
	context pc: nil.
	^ context
]

{ #category : #tests }
ContextDebuggingTest >> testStepToSendOrReturn [
	| context |
	context := self deadContext.
	self assert: context isDead.
	context := context stepToSendOrReturn.
	self assert: context isDead
]
