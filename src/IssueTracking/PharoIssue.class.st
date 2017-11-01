"
I represent a Pharo development issue.

Responsibilities:
- create text templates (e.g. for the mailing list or issue tracker comments) for common events like issue creation and slice submission
- communicate with the issue tracker

Collaborators:
- ZnClient - my link to the issue tracker
- MCSliceInfo - I can get you one if you need it

Public API and Key Messages:
Most of my behavior is illustrated in my tests. The noticable exception are my text template creation messages, the testing of which seemed to hurt as much in duplication as it added in QA.
"
Class {
	#name : #PharoIssue,
	#superclass : #Object,
	#instVars : [
		'number',
		'title'
	],
	#category : 'IssueTracking'
}

{ #category : #'instance creation' }
PharoIssue class >> number: anInteger [ 
	^ self new
		number: anInteger;
		yourself
]

{ #category : #private }
PharoIssue >> changesFromSliceCommitMessage [

	| messageHeaderLines changesLines |
	messageHeaderLines := 3.
	changesLines := self sliceCommitMessage lines allButFirst: messageHeaderLines.
	^ changesLines joinUsing: Character cr
]

{ #category : #communicating }
PharoIssue >> creationNotification [
	^ String streamContents: [ :str |
		str nextPutAll: self headerString;
		cr;
		print: self trackerUrl ]
]

{ #category : #private }
PharoIssue >> downloadTitle [
	^ ZnClient new
		url: 'http://tracker.pharo.org/issues/name/', self number asString;
		enforceHttpSuccess: true;
		get.
]

{ #category : #private }
PharoIssue >> headerString [
	^ 'Issue ', self number asString, ': ', self title.
]

{ #category : #accessing }
PharoIssue >> number [
	^ number
]

{ #category : #accessing }
PharoIssue >> number: anInteger [
	number := anInteger
]

{ #category : #accessing }
PharoIssue >> slice [
	^ MCSliceInfo new
		issueNumber: self number asString;
		title: self title;
		yourself
]

{ #category : #private }
PharoIssue >> sliceCommitMessage [
	^ self slice workingCopy versionInfo message.
	
]

{ #category : #communicating }
PharoIssue >> sliceNotification [
	^ String streamContents: [ :str |
		str
			nextPutAll: 'Fix in inbox: ';
			nextPutAll: self slice workingCopy versionInfo name;
		cr;
		cr;
		nextPutAll: self changesFromSliceCommitMessage ]
]

{ #category : #accessing }
PharoIssue >> title [

	^ title ifNil: [ title := self downloadTitle ]
]

{ #category : #accessing }
PharoIssue >> title: aString [
	title := aString
]

{ #category : #accessing }
PharoIssue >> trackerUrl [
	^ ('https://pharo.fogbugz.com/default.asp?', self number asString) asUrl
	
]
