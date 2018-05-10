"
I am not a test! I provide usage examples for the SUnit debugger.
"
Class {
	#name : #GTSUnitExampleWithSetUpAndNoTearDownTest,
	#superclass : #TestCase,
	#instVars : [
		'a'
	],
	#category : #'GT-SUnitDebugger'
}

{ #category : #testing }
GTSUnitExampleWithSetUpAndNoTearDownTest class >> isAbstract [
	^ true
]

{ #category : #testing }
GTSUnitExampleWithSetUpAndNoTearDownTest class >> isUnitTest [
	^ false
]

{ #category : #running }
GTSUnitExampleWithSetUpAndNoTearDownTest >> setUp [
	super setUp.
	a := 42.
]

{ #category : #testing }
GTSUnitExampleWithSetUpAndNoTearDownTest >> testWithErrorInSetup [
	self assert: a equals: 41
]
