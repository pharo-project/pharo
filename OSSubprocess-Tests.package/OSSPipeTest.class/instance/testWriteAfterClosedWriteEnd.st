testing
testWriteAfterClosedWriteEnd
	| pipe writeStream string |
	string := 'this is a testing string'. 
	pipe := self blockingPipe.
	pipe closeWriter.
	self should: [ pipe writer nextPutAll: string; flush ] raise: Error.
	