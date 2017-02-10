tests
testDetectNoChange

	classFactory newClass.
	monitor log store flush.
	detector := EpLostChangesDetector new.
	detector monitorLog: monitor log.
	self assert: detector lostChanges isEmpty.
	self assert: detector monitorLog entries size equals: 2. "Just to be sure of the assumed precondition: category and only one class created"
