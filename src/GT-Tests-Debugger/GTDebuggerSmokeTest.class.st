Class {
	#name : #GTDebuggerSmokeTest,
	#superclass : #TestCase,
	#instVars : [
		'session',
		'window',
		'debugger'
	],
	#category : #'GT-Tests-Debugger'
}

{ #category : #testing }
GTDebuggerSmokeTest class >> isAbstract [
	^ self name = #GTDebuggerSmokeTest
]

{ #category : #asserting }
GTDebuggerSmokeTest >> assertDebuggerClosing [
	
 	debugger close.

	self assert: window model equals: nil.
	self assert: debugger session interruptedContext equals: nil.
	self assert: debugger session interruptedProcess equals: nil.
]

{ #category : #asserting }
GTDebuggerSmokeTest >> assertDebuggerInitialization [
	
	self assert: (debugger class handlesContext: session interruptedContext).
	
	window := debugger openOn: session.
	
	self assert: window model equals: debugger.
	self assert: debugger session interruptedContext notNil.
	self assert: debugger session interruptedProcess notNil.
	
]

{ #category : #asserting }
GTDebuggerSmokeTest >> assertDebuggerInitializationForContext: aContext andStepBlock: aBlock [

	self 
		initializeSessionForContext: aContext
		withStepBlock: aBlock.

	self assertDebuggerInitialization.
]

{ #category : #accessing }
GTDebuggerSmokeTest >> debuggerToTest [
	self subclassResponsibility
]

{ #category : #initialization }
GTDebuggerSmokeTest >> initializeSessionForContext: aContext withStepBlock: aBlock [
	| process |

	process := Process 
		forContext: aContext
		priority: Processor userInterruptPriority.
	
	aBlock value: process.
	session := self debuggerToTest spanNewSessionLabeled:  'test session' forContext: process suspendedContext fromProcess: process
]

{ #category : #running }
GTDebuggerSmokeTest >> setUp [
	super setUp.
	debugger := self debuggerToTest new.
]

{ #category : #running }
GTDebuggerSmokeTest >> tearDown [ 
	debugger ifNotNil: [ debugger close].
	window := nil.
	debugger := nil.
	session := nil.
	super tearDown
]
