tests - some
testAddWithOccurences
 	| e |
	e := self element.
	empty add: e withOccurrences: 3.
	self assert: (empty includes: e).
	self assert: (empty occurrencesOf: e) equals: 1