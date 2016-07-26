tests - testing
testIsEmpty
	| stream |
	stream := String new writeStream.
	self assert: stream isEmpty.
	stream nextPut: $a.
	self deny: stream isEmpty.
	stream reset.
	self deny: stream isEmpty.