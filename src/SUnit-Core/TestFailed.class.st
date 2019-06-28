"
I model the fact that a test fails.
"
Class {
	#name : #TestFailed,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #testing }
TestFailed >> isFailed [
	^ true
]
