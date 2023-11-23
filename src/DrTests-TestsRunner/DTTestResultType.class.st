"
I model an abstract type of result for the run of a test case.
"
Class {
	#name : 'DTTestResultType',
	#superclass : 'Object',
	#category : 'DrTests-TestsRunner-Results',
	#package : 'DrTests-TestsRunner',
	#tag : 'Results'
}

{ #category : 'factory' }
DTTestResultType class >> backgroundColorStyle [
	^ self subclassResponsibility
]

{ #category : 'factory' }
DTTestResultType class >> error [
	^ DTErrorResultType new
]

{ #category : 'factory' }
DTTestResultType class >> expectedFailure [
	^ DTExpectedFailureResultType new
]

{ #category : 'factory' }
DTTestResultType class >> fail [
	^ DTFailResultType new
]

{ #category : 'factory' }
DTTestResultType class >> pass [
	^ DTPassResultType new
]

{ #category : 'factory' }
DTTestResultType class >> skipped [
	^ DTSkippedResultType new
]

{ #category : 'factory' }
DTTestResultType class >> textColor [
	^ TestResult defaultColorText
]

{ #category : 'factory' }
DTTestResultType class >> unexpectedPass [
	^ DTUnexpectedPassResultType new
]

{ #category : 'testing' }
DTTestResultType >> isError [
	^ false
]

{ #category : 'accessing' }
DTTestResultType >> isExpectedFailure [
	^ false
]

{ #category : 'testing' }
DTTestResultType >> isFail [
	^ false
]

{ #category : 'testing' }
DTTestResultType >> isPass [
	^ false
]

{ #category : 'testing' }
DTTestResultType >> isSkipped [
	^ false
]

{ #category : 'accessing' }
DTTestResultType >> isUnexpectedPass [
	^ false
]

{ #category : 'accessing' }
DTTestResultType >> name [
	^ self subclassResponsibility
]

{ #category : 'accessing' }
DTTestResultType >> pluralName [
	^ self name , 's'
]
