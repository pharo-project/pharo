tests
testContention1
	"here is a test case that breaks the standard SharedQueue from Squeak 3.8"

	| q r1 r2 |
	q := self newQueue.
	q nextPut: 10.
	q nextPut: 5.

	self should: [ q nextOrNil = 5 ].

	[ r1 := q next ] fork.
	[ r2 := q next ] fork.
	Processor  yield.   "let the above two threads block"
	
	q nextPut: 10.
	Processor yield.

	self should: [ r1 = 10 ].
	self should: [ r2 = 10 ].
	self should: [ q nextOrNil = nil ].
