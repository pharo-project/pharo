tests
testDetectOneChange

	| logWithALostChange |
	"Build a fake log with a lost change"
	classFactory newClass.
	logWithALostChange := EpLog newWithStore: monitor sessionStore store flush copyReopened refresh.
	classFactory newClass.
	monitor sessionStore flush.

	detector := EpLostChangesDetector new.
	detector monitorLog: logWithALostChange.
	self assert: detector lostChanges size equals: 1.
	self assert: detector monitorLog entries size > 1. "Just to ensure assumptions of this test"