Class {
	#name : #ContextDebuggingTest,
	#superclass : #TestCase,
	#category : #'Debugging-Utils-Tests'
}

{ #category : #tests }
ContextDebuggingTest >> deadContext [
	| semaphore context |
	semaphore := Semaphore new.
	[ context := thisContext.
	semaphore signal ] fork.
	semaphore wait.
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
