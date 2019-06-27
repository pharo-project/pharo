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
	#name : #TestResultType,
	#superclass : #Object,
	#category : #'SUnit-Core-Kernel'
}

{ #category : #factory }
TestResultType class >> newError [
	^ TestRaisedError new
]

{ #category : #factory }
TestResultType class >> newFailure [
	^ TestFailed new
]

{ #category : #factory }
TestResultType class >> newPass [
	^ TestPassed new
]

{ #category : #factory }
TestResultType class >> newSkip [
	^ TestSkipped new
]

{ #category : #factory }
TestResultType class >> newUnknown [
	^ UnknownTestResult new
]

{ #category : #factory }
TestResultType class >> newWarning [
	^ TestRaisedWarning new
]

{ #category : #accessing }
TestResultType >> color [
	"Returns the color to be used when the test is displayed to user."
	^ self subclassResponsibility
]

{ #category : #testing }
TestResultType >> isFailed [
	^ false
]

{ #category : #testing }
TestResultType >> isPassed [
	^ false
]

{ #category : #testing }
TestResultType >> isRaisedError [
	^ false
]

{ #category : #testing }
TestResultType >> isRaisedWarning [
	^ false
]

{ #category : #testing }
TestResultType >> isSkipped [
	^ false
]

{ #category : #testing }
TestResultType >> isUnknownTestResult [
	^ false
]

{ #category : #accessing }
TestResultType >> theme [
	^ Smalltalk ui theme
]
