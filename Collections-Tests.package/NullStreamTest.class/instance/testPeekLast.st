tests
testPeekLast
	self assert: stream peekLast equals: nil.
	stream nextPut: $a.
	self assert: stream peekLast equals: $a.
	stream nextPutAll: 'bcd'.
	self assert: stream peekLast equals: $d.
	stream cr.
	self assert: stream peekLast equals: Character cr.
	stream lf.
	self assert: stream peekLast equals: Character lf.
	stream space.
	self assert: stream peekLast equals: Character space.
	stream tab.
	self assert: stream peekLast equals: Character tab.
	stream next: 4 putAll: 'abcdefgh' startingAt: 3.
	self assert: stream peekLast equals: $f.