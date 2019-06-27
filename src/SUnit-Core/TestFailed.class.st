"
I model the fact that a test fails.
"
Class {
	#name : #TestFailed,
	#superclass : #TestResultType,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
TestFailed >> color [
	^ self theme warningBackgroundColor
]

{ #category : #testing }
TestFailed >> isFailed [
	^ true
]
