tests
testAddCollector6
	recorder addCollector: collector1.
	recorder addCollector: collector1.
	self assert: recorder numberOfCollectors equals: 1.