tests
testAddCycle
	aPackageCycleDetection addCycle: (DAPackageCycle new).
	self assert: (aPackageCycleDetection cycles size = 1)