tests
testDetectOneChange

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