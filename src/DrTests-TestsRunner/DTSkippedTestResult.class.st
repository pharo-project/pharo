"
I model the fact that a test was skipped.
"
Class {
	#name : #DTSkippedTestResult,
	#superclass : #DTAbstractTestResult,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTSkippedTestResult class >> backgroundColor [
	^ TestResult defaultColorBackGroundForPassingTest
]

{ #category : #testing }
DTSkippedTestResult >> isSkipped [
	<generated>
	^ true
]

{ #category : #accessing }
DTSkippedTestResult >> name [
	^ 'Skipped test'
]
