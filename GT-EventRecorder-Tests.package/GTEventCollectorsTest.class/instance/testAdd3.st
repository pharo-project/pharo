tests
testAdd3
	| collector |
	collector := GTEventCollector new.
	collectors add: collector.
	collectors add: collector.
	self assert: collectors size equals: 1.