"
An EpLostChangesDetectorTest is a test class for testing the behavior of EpLostChangesDetector
"
Class {
	#name : #EpLostChangesDetectorTest,
	#superclass : #EpEnabledIntegrationTest,
	#instVars : [
		#detector
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

	"Category addition and registration are two separate events"
	"Just to be sure of the assumed precondition: category and only one class created"	
	self assert: monitor log entriesCount equals: 3
]

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectNoChangeBecauseLogFileWasDeleted [

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
	self assertEmpty: detector lostChanges
]

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectOneChangeDetectedAndOneIgnored [

	| logWithALostChange |
	"Create an initial change that will be ignored by the detector (read below)"
	classFactory newClass.

	"Simulate the image session starts here (then previous change shouldn't be detected as lost... that's how it works)."
	logWithALostChange := EpLog newWithStore: monitor sessionStore store flush copyReopened refresh.
	classFactory newClass.
	monitor sessionStore flush.

	"At this moment, the log meets the conditions of opening an image after a crash."
	detector := EpLostChangesDetector newWithLog: logWithALostChange.

	self assert: detector hasLostChanges.
	self assert: detector lostChanges size equals: 1.
	self assert: monitor log entriesCount > 1. "Just to ensure assumptions of this test"
]

{ #category : #tests }
EpLostChangesDetectorTest >> testDetectThreeChanges [

	| logWithLostChanges numberOfLostChanges expectedLostClassNames detectedLostChanges detectedLostClassChanges |

	"Build a log with several lost changes (class additions)"
	numberOfLostChanges := 3.
	logWithLostChanges := EpLog newWithStore: monitor sessionStore store flush copyReopened refresh.
	expectedLostClassNames := (1 to: numberOfLostChanges) collect: [ :each | classFactory newClass name ] as: Array.
	monitor sessionStore flush.

	"At this moment, the log meets the conditions of opening an image after a crash."
	detector := EpLostChangesDetector newWithLog: logWithLostChanges.

	detectedLostChanges := detector lostChanges.
	self
		assert: (detectedLostChanges anySatisfy: [ : omEntry | omEntry content isEpCategoryChange ])
		description: 'It test that the class additions include a category registration'.
	detectedLostClassChanges := detectedLostChanges reject: [ : omEntry | omEntry content isEpCategoryChange ].
	detectedLostClassChanges do: [ :each | self assert: each content isEpClassChange ].

	self assert: detectedLostClassChanges size equals: numberOfLostChanges.
	self
		assert: (detectedLostClassChanges collect: [ :each | each content behaviorAffectedName ]) asArray
		equals: expectedLostClassNames
]
