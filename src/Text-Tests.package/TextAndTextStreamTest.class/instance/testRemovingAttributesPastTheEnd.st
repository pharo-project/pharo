test-errors
testRemovingAttributesPastTheEnd
	| aText |
	aText := Text string: 'foo' attribute: TextColor red .
	self should: [aText addAttribute: TextColor red from: 2 to: aText size + 1] raise: Error.