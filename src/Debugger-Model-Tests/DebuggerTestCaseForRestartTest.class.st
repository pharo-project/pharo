Class {
	#name : #DebuggerTestCaseForRestartTest,
	#superclass : #TestCase,
	#instVars : [
		'testedValue'
	],
	#category : #'Debugger-Model-Tests-Core'
}

{ #category : #running }
DebuggerTestCaseForRestartTest >> setUp [
	super setUp.
	
	"Initialize the test with some object.
	This value will change if the test is restarted.
	We use just a new object because we care about its identity"
	testedValue := Object new.
]

{ #category : #running }
DebuggerTestCaseForRestartTest >> testPushingValuesOnActivation [

	| oldValue |
	"Test case to test the debugger restart.
	
	This test will run ok if run normally.
	However, if we step into this method and restart it, the debugger will re-execute the setUp, changing the value of testedValue.
	The test should be also be green after restarting."

	oldValue := testedValue.
	self assert: oldValue == testedValue.
]

{ #category : #running }
DebuggerTestCaseForRestartTest >> testedValue [

	^ testedValue
]
