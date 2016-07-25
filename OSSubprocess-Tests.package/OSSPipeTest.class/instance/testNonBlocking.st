testing
testNonBlocking

	| pipe writeStream string |
	pipe := self nonBlockingPipe.
	self writeStuffOnThenClose: pipe.
	writeStream := self readFromAndClose: pipe writingTo: (WriteStream on: String new).
	string := (writeStream contents last: 16).
	self should: ['this is line 10*' match: string].
	pipe close
