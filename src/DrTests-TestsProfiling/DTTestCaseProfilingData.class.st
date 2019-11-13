"
I stores the information for every test.
I have the duration that is the times it takes when a test is executed. 
I also have the testCase ande the testResult.
"
Class {
	#name : #DTTestCaseProfilingData,
	#superclass : #Object,
	#instVars : [
		'testCase',
		'duration',
		'testResult'
	],
	#category : #'DrTests-TestsProfiling'
}

{ #category : #'instance creation' }
DTTestCaseProfilingData class >> testCase: testCase duration: aDuration [
	^ self new
		testCase: testCase;
		duration: aDuration;
		yourself
]

{ #category : #'instance creation' }
DTTestCaseProfilingData class >> testCase: testCase duration: aDuration testResult: aTestResult [
	^ self new
		testCase: testCase;
		duration: aDuration;
		testResult: aTestResult;
		yourself
]

{ #category : #actions }
DTTestCaseProfilingData >> drTestsBrowse [
	Smalltalk tools browser openOnClass:testCase class selector:testCase selector
]

{ #category : #accessing }
DTTestCaseProfilingData >> drTestsName [
	^ self testCase asString , ' : ', self duration asString
]

{ #category : #accessing }
DTTestCaseProfilingData >> duration [
	^ duration
]

{ #category : #accessing }
DTTestCaseProfilingData >> duration: anObject [
	duration := anObject
]

{ #category : #accessing }
DTTestCaseProfilingData >> testCase [
	^ testCase
]

{ #category : #accessing }
DTTestCaseProfilingData >> testCase: anObject [
	testCase := anObject
]

{ #category : #accessing }
DTTestCaseProfilingData >> testResult [
	^ testResult
]

{ #category : #accessing }
DTTestCaseProfilingData >> testResult: anObject [
	testResult := anObject
]
