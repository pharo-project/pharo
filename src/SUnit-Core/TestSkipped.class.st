"
I model the fact that a test was skipped during its execution.
"
Class {
	#name : #TestSkipped,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #testing }
TestSkipped >> isSkipped [
	^ true
]
