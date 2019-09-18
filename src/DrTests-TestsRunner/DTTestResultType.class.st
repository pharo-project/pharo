"
I model an abstract type of result for the run of a test case.
"
Class {
	#name : #DTTestResultType,
	#superclass : #Object,
	#category : #'DrTests-TestsRunner'
}

{ #category : #factory }
DTTestResultType class >> error [
	^ DTError new
]

{ #category : #factory }
DTTestResultType class >> fail [
	^ DTFail new
]

{ #category : #factory }
DTTestResultType class >> pass [
	^ DTPass new
]

{ #category : #factory }
DTTestResultType class >> skipped [
	^ DTSkipped new
]

{ #category : #testing }
DTTestResultType >> isError [
	<generated>
	^ false
]

{ #category : #testing }
DTTestResultType >> isFail [
	<generated>
	^ false
]

{ #category : #testing }
DTTestResultType >> isPass [
	<generated>
	^ false
]

{ #category : #testing }
DTTestResultType >> isSkipped [
	<generated>
	^ false
]

{ #category : #accessing }
DTTestResultType >> name [
	^ self subclassResponsibility
]

{ #category : #accessing }
DTTestResultType >> pluralName [
	^ self name , 's'
]
