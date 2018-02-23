"
I am not a test! I provide usage examples for the SUnit debugger.
"
Class {
	#name : #GTSUnitExampleWithSetUpAndTearDownTest,
	#superclass : #TestCase,
	#instVars : [
		'a'
	],
	#category : #'GT-SUnitDebugger'
}

{ #category : #testing }
GTSUnitExampleWithSetUpAndTearDownTest class >> isAbstract [
	^ true
]

{ #category : #testing }
GTSUnitExampleWithSetUpAndTearDownTest class >> isUnitTest [
	^ false
]

{ #category : #running }
GTSUnitExampleWithSetUpAndTearDownTest >> setUp [
	super setUp.
	a := 42.
]

{ #category : #running }
GTSUnitExampleWithSetUpAndTearDownTest >> tearDown [
	a := nil.
	super tearDown
]

{ #category : #testing }
GTSUnitExampleWithSetUpAndTearDownTest >> testWithErrorInSetup [
	self assert: a equals: 41
]
