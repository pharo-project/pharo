tests
testPack
	"Ensure that delivery pack is prepared and collector empty."
	recorder addCollector: collector1.
	collector1 add: event1.
	collector1 add: event2.
	recorder pack.
	self assert: collector1 size equals: 0.
	self assert: recorder delivery size equals: 1.