"
I model the fact that a test passes.
"
Class {
	#name : #TestPassed,
	#superclass : #TestResultType,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #accessing }
TestPassed >> color [
	^ self theme successBackgroundColor
]

{ #category : #testing }
TestPassed >> isPassed [
	^ true
]
