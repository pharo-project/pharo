tests
testAdd
	collectors add: GTEventCollector new.
	self assert: collectors size equals: 1.