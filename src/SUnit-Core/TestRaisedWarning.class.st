"
I model the fact that a test raised a Warning.
"
Class {
	#name : #TestRaisedWarning,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #testing }
TestRaisedWarning >> isRaisedWarning [
	^ true
]
