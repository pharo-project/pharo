Class {
	#name : #TestFailureTestCase,
	#superclass : #TestCase,
	#category : #'SUnit-Tests-Core'
}

{ #category : #tests }
TestFailureTestCase >> testCaptureIfSignalledWhenStepping [
	self assert: TestFailure captureIfSignalledWhenStepping.
	self deny: (TestFailure allSubclasses anySatisfy: [:c|c captureIfSignalledWhenStepping])
]
