Class {
	#name : #PharoIssueTest,
	#superclass : #TestCase,
	#instVars : [
		'issue',
		'issueNumber',
		'madeUpTitleString'
	],
	#category : #'IssueTracking-Tests'
}

{ #category : #accessing }
PharoIssueTest class >> defaultTimeLimit [
	^1 minutes
]

{ #category : #running }
PharoIssueTest >> setUp [
	super setUp.
	issueNumber := 6489.
	issue := PharoIssue number: issueNumber.
	madeUpTitleString := 'A title'.
]

{ #category : #tests }
PharoIssueTest >> testAutoTitle [
	| actualTitleFromTracker |
	actualTitleFromTracker := 'Spec-ify DropListModel / TextModel'.
	self assert: issue title equals: actualTitleFromTracker
]

{ #category : #tests }
PharoIssueTest >> testCreating [
	PharoIssue number: 6489.
]

{ #category : #'tests-implementation' }
PharoIssueTest >> testHeaderString [
	self assert: issue headerString equals: 'Issue 6489: Spec-ify DropListModel / TextModel'
]

{ #category : #tests }
PharoIssueTest >> testNumber [
	self assert: issue number equals: issueNumber
]

{ #category : #tests }
PharoIssueTest >> testSettingTitle [
	issue title: madeUpTitleString.
	self assert: issue title equals: madeUpTitleString
]

{ #category : #tests }
PharoIssueTest >> testSlice [
	self assert: (issue slice isKindOf: MCSliceInfo)
]

{ #category : #tests }
PharoIssueTest >> testTrackerUrl [
	self assert: issue trackerUrl equals: ('https://pharo.fogbugz.com/default.asp?', issueNumber asString) asUrl
]
