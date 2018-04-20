Class {
	#name : #TestMakesNoAssertion,
	#superclass : #TestFailure,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #'camp smalltalk' }
TestMakesNoAssertion >> isResumable [
	^ true
]

{ #category : #'camp smalltalk' }
TestMakesNoAssertion >> sunitAnnounce: aTestCase toResult: aTestResult [
	"Add the assertless test case to the set of test cases making no assertion."
	aTestResult addMakesNoAssertion: aTestCase.
		
	self resume
]
