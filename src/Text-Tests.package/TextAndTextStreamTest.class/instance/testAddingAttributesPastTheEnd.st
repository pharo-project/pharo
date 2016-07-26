test-errors
testAddingAttributesPastTheEnd
	| aText |
	aText := 'foo' asText.
	self should: [aText addAttribute: TextColor red from: 2 to: aText size + 1] raise: Error.