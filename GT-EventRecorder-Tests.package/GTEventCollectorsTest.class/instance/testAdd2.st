tests
testAdd2
	collectors add: GTEventCollector new.
	collectors add: GTEventCollector new.
	self assert: collectors size equals: 2.