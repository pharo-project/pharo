test-errors
testAddingAttributesBeforeBeg
	| aText |
	aText := 'foo' asText.
	self should: [aText addAttribute: TextColor red from: 0 to: aText size] raise: Error.