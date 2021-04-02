"
I model the fact that a test passed.
"
Class {
	#name : #DTPassTestResult,
	#superclass : #DTAbstractTestResult,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTPassTestResult class >> backgroundColor [
	^ TestResult defaultColorBackGroundForPassingTest
]

{ #category : #testing }
DTPassTestResult >> isPass [
	<generated>
	^ true
]

{ #category : #accessing }
DTPassTestResult >> name [
	^ 'Passing test'
]
