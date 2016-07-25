tests
testPack2
	"Ensure that there is only one delivery pack when collectors are of the same URL and category."
	recorder addCollector: collector1.
	collector1 add: event1.
	collector1 add: event2.
	recorder addCollector: collector2.
	collector2 add: event3.
	collector2 add: event4.
	recorder pack.
	self assert: collector1 size equals: 0.
	self assert: collector2 size equals: 0.
	self assert: recorder delivery size equals: 1.