tests - testing
testIsEmpty
	| stream |
	stream := ReadWriteStream on: String new.
	self assert: stream isEmpty.
	stream nextPut: $a.
	self deny: stream isEmpty.
	stream reset.
	self deny: stream isEmpty.
	stream next.
	self deny: stream isEmpty.