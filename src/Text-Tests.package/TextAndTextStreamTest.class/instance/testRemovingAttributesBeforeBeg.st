test-errors
testRemovingAttributesBeforeBeg
	| aText |
	aText := Text string: 'foo' attribute: TextColor red .
	self should: [aText removeAttribute: TextColor red from: 0 to: aText size] raise: Error.