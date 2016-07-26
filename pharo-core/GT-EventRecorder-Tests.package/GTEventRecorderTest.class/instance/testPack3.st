tests
testPack3
	"Ensure that empty collector is not delivered."
	recorder addCollector: collector1.
	self assert: collector1 size equals: 0.
	recorder pack.
	self assert: collector1 size equals: 0.
	self assert: recorder delivery size equals: 0.