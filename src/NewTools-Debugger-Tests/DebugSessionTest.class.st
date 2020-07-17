Class {
	#name : #DebugSessionTest,
	#superclass : #TestCase,
	#instVars : [
		'session'
	],
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #helpers }
DebugSessionTest >> configureSessionWithAssertionFailure [
	session := StTestDebuggerProvider new
		debuggerWithFailingAssertionContext session
]

{ #category : #helpers }
DebugSessionTest >> configureSessionWithTestFailure [
	session := StTestDebuggerProvider new debuggerWithFailingTestContext
		session
]

{ #category : #running }
DebugSessionTest >> tearDown [ 
	session ifNotNil:[session terminate].
	super tearDown
]

{ #category : #tests }
DebugSessionTest >> testIsInterruptedContextATestFailure [
	self configureSessionWithTestFailure.
	self assert: session isInterruptedContextAnAssertionFailure
]

{ #category : #tests }
DebugSessionTest >> testIsInterruptedContextAnAssertionFailure [
	self configureSessionWithAssertionFailure.
	self assert: session isInterruptedContextAnAssertionFailure
]
