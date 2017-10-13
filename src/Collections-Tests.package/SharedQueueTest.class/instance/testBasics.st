tests
testBasics
	| q |
	q := SharedQueue new.
	self should: [ q nextOrNil isNil ].
	self assert: q atEnd.
	q nextPut: 5.
	self assertCollection: q contents hasSameElements: #(5).
	self should: [ q nextOrNil = 5 ].
	self should: [ q nextOrNil isNil ].
	self assert: q atEnd.