testing
testBasics
	| q |
	q := SharedQueue new.
	self should: [ q nextOrNil isNil ].
	q nextPut: 5.
	self should: [ q nextOrNil = 5 ].
	self should: [ q nextOrNil isNil ]