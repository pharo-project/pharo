tests
testAddCollector4
	recorder addCollector: collector1.
	collector1 add: event1.
	collector1 add: event2.
	recorder delivery packAndDeliver: false.
	"wait"
	self assert: collector1 size equals: 0.
	