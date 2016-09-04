tests
testIsEmpty
	| stream |
	stream := self newStream.
	self assert: stream isEmpty.
	stream nextPut: $a.
	self deny: stream isEmpty.
	stream reset.
	self deny: stream isEmpty.