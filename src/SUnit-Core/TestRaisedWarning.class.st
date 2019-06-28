"
I model the fact that a test raised a Warning.
"
Class {
	#name : #TestRaisedWarning,
	#superclass : #TestResultSeverity,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
TestRaisedWarning >> color [
	"Same color as failure to be retro-compatible."
	^ self theme warningBackgroundColor
]

{ #category : #testing }
TestRaisedWarning >> isRaisedWarning [
	^ true
]
