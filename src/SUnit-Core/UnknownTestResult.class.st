"
I am the implementation of the null object design pattern.

I allow to represent the fact that the test hasn't been run yet.

Thus I am an unknown result.
"
Class {
	#name : #UnknownTestResult,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #testing }
UnknownTestResult >> isUnknownTestResult [
	^ true
]
