tests
testAddCollector5
	recorder addCollector: GTEventCollector new.
	recorder addCollector: GTEventCollector new.
	self assert: recorder numberOfCollectors equals: 2.