tests
testBasics
	| q |
	q := self newQueue.
	
	q nextPut: 5.
	
	self assert: q next = 5.
	
	q nextPut: 10.
	
	self assert: q nextOrNil = 10.
	
	self assert: q nextOrNil == nil