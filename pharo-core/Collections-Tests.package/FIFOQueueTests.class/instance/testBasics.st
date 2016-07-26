tests
testBasics
	| q |
	q := self newQueue.
	
	q nextPut: 5.
	
	self assert: q peek = 5.
	self assert: q nextOrNil = 5.
	
	q nextPut: 10.
	q nextPut: 15.
	
	self assert: q nextOrNil = 10.
	self assert: q peek = 15.
	self assert: q nextOrNil = 15.
	
	self assert: q nextOrNil == nil