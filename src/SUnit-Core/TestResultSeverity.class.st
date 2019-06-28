"
I reify the type of a test result.
My subclasses provide concrete types of test results such as:
- Pass
- Failure
- Error
- Skipped
- ...

Having real objects to model the type of a result allow to delegate some tasks to them such as deciding the color to display for a test.
"
Class {
	#name : #TestResultSeverity,
	#superclass : #Object,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #factory }
TestResultSeverity class >> newError [
	^ TestRaisedError new
]

{ #category : #factory }
TestResultSeverity class >> newFailure [
	^ TestFailed new
]

{ #category : #factory }
TestResultSeverity class >> newPass [
	^ TestPassed new
]

{ #category : #factory }
TestResultSeverity class >> newSkip [
	^ TestSkipped new
]

{ #category : #factory }
TestResultSeverity class >> newUnknown [
	^ UnknownTestResult new
]

{ #category : #factory }
TestResultSeverity class >> newWarning [
	^ TestRaisedWarning new
]

{ #category : #comparing }
TestResultSeverity >> = aTestResultType [
	"TestResultType objects are usually stateless, so they are equal if their classes are equal."
	^ self class = aTestResultType class
]

{ #category : #comparing }
TestResultSeverity >> hash [
	"TestResultType objects are usually stateless, so the hash is the class' hash."
	^ self class hash
]

{ #category : #testing }
TestResultSeverity >> isFailed [
	^ false
]

{ #category : #testing }
TestResultSeverity >> isPassed [
	^ false
]

{ #category : #testing }
TestResultSeverity >> isRaisedError [
	^ false
]

{ #category : #testing }
TestResultSeverity >> isRaisedWarning [
	^ false
]

{ #category : #testing }
TestResultSeverity >> isSkipped [
	^ false
]

{ #category : #testing }
TestResultSeverity >> isUnknownTestResult [
	^ false
]

{ #category : #accessing }
TestResultSeverity >> theme [
	^ Smalltalk ui theme
]
