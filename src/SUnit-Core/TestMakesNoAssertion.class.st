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
	"either tally separately, or tally as a failure"

	aTestCase class assertionlessTestsShouldFail
		ifTrue: [ aTestResult addFailure: aTestCase ]
		ifFalse: [ aTestResult addMakesNoAssertion: aTestCase ]
]
