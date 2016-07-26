tests
testConsolidated
	"Test that all collectors are consolidated into one."
	collector1 add: data first.
	collector2 add: data second.
	collector3 add: data third.
	consolidate collectors: collectors.
	consolidated := consolidate consolidated.
	self assert: consolidated size equals: 1.
	self assert: consolidated first bundle asArray equals: data.