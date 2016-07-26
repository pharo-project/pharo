tests
testCounting
	| expectedPosition |
	expectedPosition := 0.
	self assert: stream position equals: expectedPosition.
	
	stream nextPut: $a.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream nextPutAll: 'bcd'.
	expectedPosition := expectedPosition + 3.
	self assert: stream position equals: expectedPosition.
	
	stream cr.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream lf.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream space.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream tab.
	expectedPosition := expectedPosition + 1.
	self assert: stream position equals: expectedPosition.
	
	stream next: 4 putAll: 'abcdefgh' startingAt: 3.
	expectedPosition := expectedPosition + 4.
	self assert: stream position equals: expectedPosition.