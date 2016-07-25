testing
testReadAfterClosedReadEnd
	| pipe writeStream readStream string |
	string := 'this is a testing string'. 
	pipe := self blockingPipe.
	pipe writer nextPutAll: string; flush.
	readStream := pipe reader.
	pipe closeReader.
	self assert: readStream closed.
	self should: [ readStream upToEndOfFile ] raise: Error.
	self should: [ readStream upToEnd ] raise: Error.
	