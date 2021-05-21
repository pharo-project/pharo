Class {
	#name : #StDebuggerAssertionContextTest,
	#superclass : #TestCase,
	#instVars : [
		'session'
	],
	#category : #'NewTools-Debugger-Tests-Model'
}

{ #category : #running }
StDebuggerAssertionContextTest >> tearDown [
	session ifNotNil:[session clear].
	super tearDown 
]

{ #category : #tests }
StDebuggerAssertionContextTest >> testFindFailedAssertionContextInSenderChain [
	| debugger |
	debugger := StTestDebuggerProvider new debuggerWithFailingAssertionContext.
	session := debugger session.
	self assert: session interruptedContext findFailedAssertionContextInSenderChain notNil.
	session clear.
	debugger := StTestDebuggerProvider new debuggerWithDNUContext. 
	session := debugger session.
	self should: [ session interruptedContext findFailedAssertionContextInSenderChain ] raise: StDebuggeAssertionFailureContextNotFound.
	
]

{ #category : #tests }
StDebuggerAssertionContextTest >> testFindFailedAssertionResultsInSenderChain [
	| debugger assertionResult|
	debugger := StTestDebuggerProvider new debuggerWithFailingAssertionContext.
	session := debugger session.
	assertionResult := session interruptedContext findFailedAssertionResultsInSenderChain.
	
	self assert: assertionResult actual equals: 4.
	self assert: assertionResult expected equals: 2.
	
	session clear.
	debugger := StTestDebuggerProvider new debuggerWithDNUContext. 
	session := debugger session.
	self should: [session interruptedContext findFailedAssertionResultsInSenderChain] raise: StDebuggeAssertionFailureContextNotFound.
	
]
