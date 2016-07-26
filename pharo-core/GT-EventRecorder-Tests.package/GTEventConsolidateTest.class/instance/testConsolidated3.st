tests
testConsolidated3
	"Test that all collectors are consolidated according to URL."
	collector1 add: data first; url: 'http://localhost/1' asZnUrl.
	collector2 add: data second; url: 'http://localhost/2' asZnUrl.
	collector3 add: data third; url: 'http://localhost/1' asZnUrl.
	consolidate collectors: collectors.
	consolidated := consolidate consolidated.
	self assert: consolidated size equals: 2.
	self assert: consolidated first bundle asArray equals: {data first. data third}.
	self assert: consolidated second bundle asArray equals: {data second}.
	