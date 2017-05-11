tests
testDetectInEmptyLog

	detector := EpLostChangesDetector newWithLog: monitor log.
	self deny: detector hasLostChanges.
	self assert: detector lostChanges isEmpty.

	self assert: monitor log entries isEmpty. "Just to be sure of the assumed precondition"
