"
An EpLostChangesDetectorTest is a test class for testing the behavior of EpLostChangesDetector
"
Class {
	#name : #EpLostChangesDetectorTest,
	#superclass : #EpEnabledIntegrationTest,
	#instVars : [
		'detector'
	],
	#category : #'EpiceaBrowsers-Tests-Integration'
}

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectInEmptyLog [
	detector := EpLostChangesDetector newWithLog: monitor log.
	self deny: detector hasLostChanges.
	self assertEmpty: detector lostChanges.

	self assertEmpty: monitor log entries	"Just to be sure of the assumed precondition"
]

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectNoChange [
	classFactory newClass.
	monitor log store flush.
	detector := EpLostChangesDetector newWithLog: monitor log.
	self deny: detector hasLostChanges.
	self assertEmpty: detector lostChanges.

	self assert: monitor log entriesCount equals: 2	"Just to be sure of the assumed precondition: category and only one class created"
]

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectOneChange [

	| logWithALostChange |
	"Build a fake log with a lost change"
	classFactory newClass.
	logWithALostChange := EpLog newWithStore: monitor sessionStore store flush copyReopened refresh.
	classFactory newClass.
	monitor sessionStore flush.

	detector := EpLostChangesDetector newWithLog: logWithALostChange.
	self assert: detector hasLostChanges.
	self assert: detector lostChanges size equals: 1.

	self assert: monitor log entriesCount > 1. "Just to ensure assumptions of this test"
]

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectOneChangeButFileDeleted [

	| logWithALostChange |
	"Build a fake log with a lost change"
	classFactory newClass.
	logWithALostChange := EpLog newWithStore: monitor sessionStore store flush copyReopened refresh.
	classFactory newClass.
	monitor sessionStore flush.

	"Delete the file"
	monitor sessionStore store ensureDeleteFile.

	detector := EpLostChangesDetector newWithLog: logWithALostChange.
	self deny: detector hasLostChanges.
	self assertEmpty: detector lostChanges.

]
