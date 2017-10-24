tests
testNextOrNilSuchThat
	| q item |
	q := SharedQueue new.
	q nextPut: 5.
	q nextPut: 6.
	item := q findFirst: [ :x | x even ].
	self should: [ item = 6 ].
	self should: [ q nextOrNil = 5 ].
	self should: [ q nextOrNil isNil ]