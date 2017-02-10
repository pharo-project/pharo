tests
testDetectInEmptyLog

	detector := EpLostChangesDetector new.
	detector monitorLog: monitor log.
	self assert: detector lostChanges isEmpty.
	self assert: detector monitorLog entries isEmpty. "Just to be sure of the assumed precondition"
