tests
testNextOrNilSuchThat
	| q item |
	q := self newQueue.
	q nextPut: 5.
	q nextPut: 6.

	item := q nextOrNilSuchThat: [ :x | x even ].
	self should: [ item = 6 ].

	self should: [ q nextOrNil = 5 ].
	self should: [ q nextOrNil = nil ].
