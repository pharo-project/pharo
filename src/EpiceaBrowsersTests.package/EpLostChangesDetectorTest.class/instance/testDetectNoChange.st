tests
testDetectNoChange

	classFactory newClass.
	monitor log store flush.
	detector := EpLostChangesDetector newWithLog: monitor log.
	self deny: detector hasLostChanges.
	self assert: detector lostChanges isEmpty.

	self assert: monitor log entriesCount equals: 2. "Just to be sure of the assumed precondition: category and only one class created"
