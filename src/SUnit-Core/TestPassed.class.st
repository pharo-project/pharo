"
I model the fact that a test passes.
"
Class {
	#name : #TestPassed,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #testing }
TestPassed >> isPassed [
	^ true
]
