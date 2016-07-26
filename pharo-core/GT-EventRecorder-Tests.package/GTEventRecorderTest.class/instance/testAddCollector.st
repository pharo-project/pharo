tests
testAddCollector
	recorder addCollector: GTEventCollector new.
	self assert: recorder numberOfCollectors equals: 1.