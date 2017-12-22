"
This error is raised when a test case did not make any assertion.
"
Class {
	#name : #TestMakesNoAssertion,
	#superclass : #TestFailure,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #'camp smalltalk' }
TestMakesNoAssertion >> sunitAnnounce: aTestCase toResult: aTestResult [
	"First, add the assertless test case to the set of test cases making no assertion."
	aTestResult addMakesNoAssertion: aTestCase.
	"Then, if assertless tests should fail, also add it to failures set."
	aTestCase class assertionlessTestsShouldFail
		ifTrue: [ aTestResult addFailure: aTestCase ]
]
