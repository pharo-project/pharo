"
I model an abstract type of result for the run of a test case.
"
Class {
	#name : #DTAbstractTestResult,
	#superclass : #Object,
	#category : #'DrTests-TestsRunner-Results'
}

{ #category : #factory }
DTAbstractTestResult class >> backgroundColor [
	^ self subclassResponsibility
]

{ #category : #factory }
DTAbstractTestResult class >> error [
	^ DTErrorTestResult new
]

{ #category : #factory }
DTAbstractTestResult class >> fail [
	^ DTFailTestResult new
]

{ #category : #factory }
DTAbstractTestResult class >> pass [
	^ DTPassTestResult new
]

{ #category : #factory }
DTAbstractTestResult class >> skipped [
	^ DTSkippedTestResult new
]

{ #category : #factory }
DTAbstractTestResult class >> textColor [
	^ TestResult defaultColorText
]

{ #category : #testing }
DTAbstractTestResult >> isError [
	<generated>
	^ false
]

{ #category : #testing }
DTAbstractTestResult >> isFail [
	<generated>
	^ false
]

{ #category : #testing }
DTAbstractTestResult >> isPass [
	<generated>
	^ false
]

{ #category : #testing }
DTAbstractTestResult >> isSkipped [
	<generated>
	^ false
]

{ #category : #accessing }
DTAbstractTestResult >> name [
	^ self subclassResponsibility
]

{ #category : #accessing }
DTAbstractTestResult >> pluralName [
	^ self name , 's'
]
