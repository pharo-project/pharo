"
This class mixes two concerns (for conciseness).
It tests that a test case failing in the tear down does indeed clean up its instance variables.
It does so by making an instance of itself and testing it.

The test can be executed with shouldFailTearDown true or false, with default false.
If true, it will fail during tearDown.
If false, it will not fail.
"
Class {
	#name : 'FailingTearDownTest',
	#superclass : 'TestCase',
	#instVars : [
		'shouldFailTearDown',
		'assignedVariableToTestCleanup'
	],
	#category : 'SUnit-Tests-Core',
	#package : 'SUnit-Tests',
	#tag : 'Core'
}

{ #category : 'running' }
FailingTearDownTest >> failTearDown [

	shouldFailTearDown := true
]

{ #category : 'running' }
FailingTearDownTest >> isClean [

	^ assignedVariableToTestCleanup isNil
]

{ #category : 'running' }
FailingTearDownTest >> setUp [

	super setUp.
	shouldFailTearDown := false
]

{ #category : 'running' }
FailingTearDownTest >> tearDown [

	shouldFailTearDown ifTrue: [ self error: 'Failing to test the tearDown' ].
	super tearDown
]

{ #category : 'running' }
FailingTearDownTest >> testAssignToTestCleanup [

	"
	This test is not meant to assert something but to be used by #testFailingTearDown.
	It will just assign a variable to the current test instance so we can test the further cleanup.
	See the class comment for more details
	"
	assignedVariableToTestCleanup := 'some value'
]

{ #category : 'running' }
FailingTearDownTest >> testFailingTearDown [

	| testCase |
	testCase := self class selector: #testAssignToTestCleanup.
	testCase failTearDown.

	testCase run.

	self assert: testCase isClean
]
