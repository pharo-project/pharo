tests
testConsolidated4
	"Test that all collectors are consolidated according to URL."
	collector1 add: data first; category: #c1.
	collector2 add: data second; category: #c2.
	collector3 add: data third; category: #c1.
	consolidate collectors: collectors.
	consolidated := consolidate consolidated.
	self assert: consolidated size equals: 2.
	self assert: consolidated first bundle asArray equals: {data first. data third}.
	self assert: consolidated second bundle asArray equals: {data second}.
	