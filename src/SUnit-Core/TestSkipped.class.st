"
I model the fact that a test was skipped during its execution.
"
Class {
	#name : #TestSkipped,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
TestSkipped >> color [
	^ self theme disabledColor
]

{ #category : #testing }
TestSkipped >> isSkipped [
	^ true
]
