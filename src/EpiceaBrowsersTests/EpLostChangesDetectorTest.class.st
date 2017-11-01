"
An EpLostChangesDetectorTest is a test class for testing the behavior of EpLostChangesDetector
"
Class {
	#name : #EpLostChangesDetectorTest,
	#superclass : #EpEnabledIntegrationTest,
	#instVars : [
		'detector'
	],
	#category : 'EpiceaBrowsersTests-Integration'
}

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectInEmptyLog [

	detector := EpLostChangesDetector newWithLog: monitor log.
	self deny: detector hasLostChanges.
	self assert: detector lostChanges isEmpty.

	self assert: monitor log entries isEmpty. "Just to be sure of the assumed precondition"

]

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectNoChange [

	classFactory newClass.
	monitor log store flush.
	detector := EpLostChangesDetector newWithLog: monitor log.
	self deny: detector hasLostChanges.
	self assert: detector lostChanges isEmpty.

	self assert: monitor log entriesCount equals: 2. "Just to be sure of the assumed precondition: category and only one class created"

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
