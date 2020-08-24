"
A DebugRequestTest is a test class for testing the behavior of DebugRequest
"
Class {
	#name : #OupsDebugRequestTest,
	#superclass : #TestCase,
	#category : #'Oups-Tests-tests - infrastructure'
}

{ #category : #tests }
OupsDebugRequestTest >> testCreationFromException [
	| error dr |
	[ self error ] on: Error do: [ :e | error := e].
	dr := OupsDebugRequest newForException: error.
	self assert: dr exception identicalTo: error.
	self assert: dr process identicalTo: dr defaultProcess
]

{ #category : #tests }
OupsDebugRequestTest >> testDebugSession [
	| error dr ds |
	[ self error ] on: Error do: [ :e | error := e].
	dr := OupsDebugRequest newForException: error.
	ds := dr debugSession.
	self assert: ds interruptedContext identicalTo: error signalerContext.
	self assert: ds interruptedProcess identicalTo: dr process.
	self assert: ds exception identicalTo: error.
]

{ #category : #tests }
OupsDebugRequestTest >> testDefaultProcess [
	| dr |
	dr := OupsDebugRequest basicNew.
	self assert: dr process identicalTo: Processor activeProcess
	
]

{ #category : #tests }
OupsDebugRequestTest >> testNewNotAllowed [
	self should: [ OupsDebugRequest new ] raise: ShouldNotImplement
]

{ #category : #tests }
OupsDebugRequestTest >> testNoPreDebugActionsBeforeComputation [
	| dr |
	dr := OupsDebugRequest basicNew.
	self assert: dr preDebugActions isEmpty
]

{ #category : #tests }
OupsDebugRequestTest >> testNullExceptionForNamed [
	| context name nullException |
	context := thisContext.
	name := 'testName'.
	nullException := OupsDebugRequest basicNew nullExceptionFor: context named: name.
	self assert: nullException signalerContext identicalTo: context.
	self assert: nullException messageText equals: name.
]

{ #category : #tests }
OupsDebugRequestTest >> testPreDebugActionForDebugItMethod [
	| dr |
	dr := OupsDebugRequest basicNew.
	dr compiledMethod: OupsDebugRequestTest>>#testPreDebugActionForDebugItMethod.
	dr computePreDebugActions.
	self assert: dr preDebugActions isNotEmpty
]
