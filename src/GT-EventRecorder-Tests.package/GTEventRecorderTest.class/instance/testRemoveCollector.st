tests
testRemoveCollector
	recorder removeCollector: GTEventCollector new.
	self assert: recorder numberOfCollectors equals: 0.