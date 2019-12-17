"
A GlobalIdentifierMergerTest is a test class for testing the behavior of GlobalIdentifierMerger
"
Class {
	#name : #GlobalIdentifierMergerTest,
	#superclass : #TestCase,
	#instVars : [
		'existing',
		'stored',
		'merger'
	],
	#category : #'System-Identification-Tests'
}

{ #category : #running }
GlobalIdentifierMergerTest >> setUp [
	super setUp.
	existing := Dictionary new.
	stored := Dictionary new.
	merger := GlobalIdentifierMerger new.
	merger existing: existing; stored: stored
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge [
	"Existing is empty. Stored has value."
	stored at: #a put: 42.
	self assert: merger merge.
	self assert: existing equals: stored.
	self assert: (existing at: #a) equals: 42.
	
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge2 [
	"Existing has something. Stored has another key."

	existing at: #a put: 42.
	stored at: #b put: 21.
	self assert: merger merge.
	self deny: existing equals: stored.
	self assert: existing size equals: 2.
	self assert: stored size equals: 1.
	self assert: (existing at: #a) equals: 42.
	self assert: (existing at: #b) equals: 21
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge3 [
	"Existing has same key as stored."

	existing at: #a put: 42.
	stored at: #a put: 21.
	self deny: merger merge.
	self deny: existing equals: stored.
	self assert: existing size equals: 1.
	self assert: stored size equals: 1.
	self assert: (existing at: #a) equals: 42
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge4 [
	"Existing has different computer id as stored."

	existing at: #computerUUID put: 42.
	existing at: #a put: 43.
	existing at: #c put: 44.
	stored at: #computerUUID put: 21.
	stored at: #b put: 22.
	stored at: #c put: 23.
	self assert: merger merge.
	self deny: existing equals: stored.
	self assert: existing size equals: 4.
	self assert: stored size equals: 3.
	self assert: (existing at: #computerUUID) equals: 21 asString.
	self assert: (existing at: #a) equals: 43.
	self assert: (existing at: #b) equals: 22.
	self assert: (existing at: #c) equals: 23
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge5 [
	"Existing has different computer id as stored."

	existing at: #computerUUID put: 42.
	existing at: #a put: 43.
	existing at: #c put: 44.
	stored at: #b put: 22.
	stored at: #c put: 23.
	self assert: merger merge.
	self deny: existing equals: stored.
	self assert: existing size equals: 4.
	self assert: stored size equals: 2.
	self assert: (existing at: #computerUUID) equals: 42 asString.
	self assert: (existing at: #a) equals: 43.
	self assert: (existing at: #b) equals: 22.
	self assert: (existing at: #c) equals: 44
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge6 [
	"Existing has different computer id as stored."

	existing at: #a put: 43.
	existing at: #c put: 44.
	stored at: #computerUUID put: 21.
	stored at: #b put: 22.
	stored at: #c put: 23.
	self assert: merger merge.
	self deny: existing equals: stored.
	self assert: existing size equals: 4.
	self assert: stored size equals: 3.
	self assert: (existing at: #computerUUID) equals: 21 asString.
	self assert: (existing at: #a) equals: 43.
	self assert: (existing at: #b) equals: 22.
	self assert: (existing at: #c) equals: 23
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge7 [
	"Ensure that it converts computerUUID into string."

	stored at: #computerUUID put: UUID new.
	self assert: merger merge.
	self deny: existing equals: stored.
	self assert: (existing at: #computerUUID) equals: (stored at: #computerUUID) asString
]

{ #category : #tests }
GlobalIdentifierMergerTest >> testMerge8 [
	"Ensure that it converts secretUUID into string."

	stored at: #secretUUID put: UUID new.
	self assert: merger merge.
	self deny: existing equals: stored.
	self assert: (existing at: #secretUUID) equals: (stored at: #secretUUID) asString
]
